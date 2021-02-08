// Copyright (c) 2017-Present Pivotal Software, Inc.  All rights reserved.
//
// This file was taken from Looking Glass, which is double-licensed under
//
// the Mozilla Public License 1.1 ("MPL") and the Apache License version 2
// ("ASL").
//
// This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND,
// either express or implied. See the LICENSE file for specific language governing
// rights and limitations of this software.
//

#define NIF_FUNCTION_NAME(f) cutil_tracer_ ## f

#include "nif_helpers.h"

// List of atoms used by this NIF.
//
// @todo We don't use threads so perhaps we should make nif_helpers
// better by splitting concerns into threads/not and have nif_helpers
// decide when to create the _nif_thread_ret atom or not.

#define NIF_ATOMS(A) \
    A(_nif_thread_ret_) \
    A(call) \
    A(caller_mfa) \
    A(closed) \
    A(cpu_timestamp) \
    A(cutil_tracer_event) \
    A(discard) \
    A(exception_from) \
    A(exit) \
    A(extra) \
    A(filter) \
    A(gc_major_end) \
    A(gc_major_start) \
    A(gc_minor_end) \
    A(gc_minor_start) \
    A(getting_linked) \
    A(getting_unlinked) \
    A(in) \
    A(in_exiting) \
    A(link) \
    A(match_spec_result) \
    A(mode) \
    A(monotonic) \
    A(nil) \
    A(ok) \
    A(open) \
    A(out) \
    A(out_exited) \
    A(out_exiting) \
    A(percent) \
    A(profile) \
    A(receive) \
    A(register) \
    A(remove) \
    A(return_from) \
    A(return_to) \
    A(scheduler_id) \
    A(send) \
    A(send_to_non_existing_process) \
    A(seq_token) \
    A(spawn) \
    A(spawned) \
    A(strict_monotonic) \
    A(target_id) \
    A(timestamp) \
    A(trace) \
    A(trace_id) \
    A(trace_status) \
    A(tracers) \
    A(trigger_mfa) \
    A(unlink) \
    A(unregister)

NIF_ATOMS(NIF_ATOM_DECL)

// List of functions defined in this NIF.

#define NIF_FUNCTIONS(F) \
    F(enabled, 3) \
    F(enabled_call, 3) \
    F(enabled_procs, 3) \
    F(enabled_running_procs, 3) \
    F(enabled_send, 3) \
    F(trace, 5)

NIF_FUNCTIONS(NIF_FUNCTION_H_DECL)

ERL_NIF_TERM NIF_FUNCTION_NAME(filtered_trace)(ErlNifEnv*, const ERL_NIF_TERM [], ErlNifPid tracer);
ERL_NIF_TERM NIF_FUNCTION_NAME(regular_trace)(ErlNifEnv*, const ERL_NIF_TERM [], ErlNifPid tracer);

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    NIF_ATOMS(NIF_ATOM_INIT)

    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = *old_priv_data;

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

// enabled(TraceTag, TracerState, Tracee)

NIF_FUNCTION(enabled)
{
    ERL_NIF_TERM tracers, value;
    ErlNifPid tracer;

    // @todo We can go one step further by having the one pid
    // in its own value in the map, skipping a get_map_value step.

    // This function will only be called for trace_status.
    // We can take a few shortcuts knowing this.

    // Disable the trace when the tracers option is missing.
    if (!enif_get_map_value(env, argv[1], atom_tracers, &tracers))
        return atom_remove;

    // Because the tracers supervisor is a one_for_all, we only need
    // to check one of the tracer processes to confirm all are alive.

    // We know for a fact that this key exists because
    // there's at least one tracer process.
    enif_get_map_value(env, tracers, enif_make_int(env, 0), &value);

    // Disable the trace when one of the tracers is not a local process.
    if (!enif_get_local_pid(env, value, &tracer))
        return atom_remove;

    // Disable the trace when one of the tracers is not alive.
    if (!enif_is_process_alive(env, &tracer))
        return atom_remove;

    return atom_discard;
}

NIF_FUNCTION(enabled_call)
{
    // We always want both call and return_to.
    return atom_trace;
}

NIF_FUNCTION(enabled_procs)
{
    ERL_NIF_TERM mode;

    // We only want the spawn and exit events when 'profile' mode
    // is enabled. Technically we only care about exits for callgrind,
    // but spawn is cheap to keep and useful for message profilers.
    if (enif_get_map_value(env, argv[1], atom_mode, &mode)
        && enif_is_identical(atom_profile, mode)
        && !(enif_is_identical(atom_spawn, argv[0])
            || enif_is_identical(atom_exit, argv[0]))) {
        return atom_discard;
    }

    return atom_trace;
}

NIF_FUNCTION(enabled_running_procs)
{
    // We always want both in and out.
    return atom_trace;
}

NIF_FUNCTION(enabled_send)
{
    // We always want both send and send_to_non_existing_process.
    return atom_trace;
}



ERL_NIF_TERM NIF_FUNCTION_NAME(filtered_trace)(ErlNifEnv* env, const ERL_NIF_TERM argv[], ErlNifPid tracer)
{
    /*
    * argv[0]: TraceTag, should only be 'call'
    * argv[1]: TracerState, map containing #{mode => filter, tracers=>#{}, trace-id=>binary()}
    * argv[2]: Tracee
    * argv[3]: Message
    * argv[4]: Options, map containing `match_spec_result`
    */
    const ERL_NIF_TERM *seq_token_tuple;
    ERL_NIF_TERM ts, extra, mspec, msg, trace_id, target_id, seq_token_term, trigger_mfa, caller_mfa;
    int has_extra, has_mspec, seq_token_arity;

    // Everything good. Generate a timestamp to include in the message.

    ts = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC));

    has_extra = enif_get_map_value(env, argv[4], atom_extra, &extra);
    has_mspec = enif_get_map_value(env, argv[4], atom_match_spec_result, &mspec);

    // We only expect to use filter feature for call tracers with matchspec
    if(!has_mspec) {
        return atom_ok;
    }

    if(!enif_get_map_value(env, argv[1], atom_trace_id, &trace_id)) {
        return atom_ok;
    }

    if(!enif_get_map_value(env, mspec, atom_target_id, &target_id)) {
        return atom_ok;
    }
    if(!enif_get_map_value(env, mspec, atom_seq_token, &seq_token_term)) {
        return atom_ok;
    }
    if(!enif_get_tuple(env, seq_token_term, &seq_token_arity, &seq_token_tuple)) {
        return atom_ok;
    }
    if(seq_token_arity < 2) {
        return atom_ok;
    }
    // ignore events if trace_id doesn't match
    if(enif_compare(seq_token_tuple[1], trace_id) != 0) {
        return atom_ok;
    }
    if(!enif_get_map_value(env, mspec, atom_caller_mfa, &caller_mfa)) {
        return atom_ok;
    }
    if(!enif_get_map_value(env, mspec, atom_trigger_mfa, &trigger_mfa)) {
        return atom_ok;
    }
    if(enif_compare(trigger_mfa, caller_mfa) != 0) {
        return atom_ok;
    }


    // Build the message. There can be two different messages
    // depending on whether the extra option was set:
    //
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, nil, nil}
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, Extra, nil}
    //
    // On top of that when match specs are enabled we may have
    // one additional term at the end of the tuple containing
    // the result of the match spec function.
    //
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, nil, nil, Result}
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, nil, Extra, Result}

    if (has_extra && has_mspec) {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, extra, mspec);
    } else if (has_extra) {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, extra, atom_nil);
    } else if (has_mspec) {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, atom_nil, mspec);
    } else {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, atom_nil, atom_nil);
    }

    // Send the message to the selected tracer.
    enif_send(env, &tracer, NULL, msg);
    return atom_ok;
}

ERL_NIF_TERM NIF_FUNCTION_NAME(regular_trace)(ErlNifEnv* env, const ERL_NIF_TERM argv[], ErlNifPid tracer)
{
    /*
    * argv[0]: TraceTag, should only be 'call'
    * argv[1]: TracerState, map containing #{mode => filter, tracers=>#{}, trace-id=>binary()}
    * argv[2]: Tracee
    * argv[3]: Message
    * argv[4]: Options, map containing `match_spec_result`
    */
    ERL_NIF_TERM ts, extra, mspec, msg;
    int has_extra, has_mspec;
    // Everything good. Generate a timestamp to include in the message.

    ts = enif_make_int64(env, enif_monotonic_time(ERL_NIF_USEC));

    // Build the message. There can be two different messages
    // depending on whether the extra option was set:
    //
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, nil, nil}
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, Extra, nil}
    //
    // On top of that when match specs are enabled we may have
    // one additional term at the end of the tuple containing
    // the result of the match spec function.
    //
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, nil, nil, Result}
    // - {cutil_tracer_event, Tag, Tracee, Ts, Term, nil, nil, Extra, Result}

    has_extra = enif_get_map_value(env, argv[4], atom_extra, &extra);
    has_mspec = enif_get_map_value(env, argv[4], atom_match_spec_result, &mspec);

    if (has_extra && has_mspec) {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, extra, mspec);
    } else if (has_extra) {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, extra, atom_nil);
    } else if (has_mspec) {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, atom_nil, mspec);
    } else {
        msg = enif_make_tuple8(env, atom_cutil_tracer_event, argv[0], argv[2], ts, argv[3], atom_nil, atom_nil, atom_nil);
    }

    // Send the message to the selected tracer.

    enif_send(env, &tracer, NULL, msg);

    return atom_ok;
}

// trace(TraceTag, TracerState, Tracee, TraceTerm, Opts)

NIF_FUNCTION(trace)
{
    /*
    * argv[0]: TraceTag, should only be 'call'
    * argv[1]: TracerState, map containing #{mode => filter, tracers=>#{}, trace-id=>binary()}
    * argv[2]: Tracee
    * argv[3]: Message
    * argv[4]: Options, map containing `match_spec_result`
    */
    ERL_NIF_TERM tracers, head, mode;
    ErlNifPid tracer;
    unsigned int nth;
    size_t len;

    if (!enif_get_map_value(env, argv[1], atom_tracers, &tracers))
        return atom_ok;

    // We know for a fact that the argument is a map. And if not,
    // no problem because we will return when trying to get a value from it.
    enif_get_map_size(env, tracers, &len);

#if (ERL_NIF_MAJOR_VERSION >= 2) && (ERL_NIF_MINOR_VERSION >= 12)
    nth = enif_hash(ERL_NIF_INTERNAL_HASH, argv[2], 0) % len;
#else
    // Select the correct tracer for this process.
    //
    // The pid value is detailed in:
    //     5b6dd0e84cf0f1dc19ddd05f86cf04b2695d8a9e/erts/emulator/beam/erl_term.h#L498
    //
    // As can be seen there, the first four bits of the pid value
    // are always the same. We therefore shift them out.

    ErlNifPid tracee;

    if (!enif_get_local_pid(env, argv[2], &tracee))
        return atom_ok;

    nth = (tracee.pid >> 4) % len;
#endif

    if (!enif_get_map_value(env, tracers, enif_make_int(env, nth), &head))
        return atom_ok;

    if (!enif_get_local_pid(env, head, &tracer))
        return atom_ok;

    if (enif_get_map_value(env, argv[1], atom_mode, &mode)
        && enif_is_identical(atom_filter, mode)) {
            return NIF_FUNCTION_NAME(filtered_trace)(env, argv, tracer);
    } else {
        return NIF_FUNCTION_NAME(regular_trace)(env, argv, tracer);
    }
}

static ErlNifFunc nif_funcs[] = {
    NIF_FUNCTIONS(NIF_FUNCTION_ARRAY)
};

ERL_NIF_INIT(cutil_tracer, nif_funcs, load, NULL, upgrade, unload)