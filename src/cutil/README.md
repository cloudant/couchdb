# Description

`cutil` application is a collection of "pure" miscellaneous functions. The "pure"
in a sense that these functions should not call any other CouchDB applications.
The two main reasons for this application to exist are:

- to share helper functions between applicattions
- avoid or break cyclic dependencies between applications

# Provided functionality

## Tracing

There are cases when existent logging in the CouchDB codebase is not sufficient to debug
problem at hand. In such cases the usual approached are: create special build with extra
logging or use erlang tracing facilities. There are pros and cons to each approach.
The tracing potentially provides better time to recovery. There is a danger with using traditional Erlang tracing though. There could be only one tracing process at a time and
all trace events go to that single process. Starting from OTP 19.0 there is an alternative
solution, which is called `erl_tracer`. The `erl_traces` is a NIF which can pre-filter
events before sending them to a receiving process implemented in Erlang. Another benefit
of using `erl_tracer` approach is that the NIF can dispatch events to multiple erlang
processes, which eliminates a bottleneck.

The `cutil_trace` module in the `cutil` application provides an interface to this tracing facility. The NIF was adapted from [Looking Glass](https://github.com/rabbitmq/looking_glass). The adaptation is in the addition of a filtering mode and changing event format from
tuples with different arity to a unified erlang record. The original project supports
profiling of erlang functions. The profiling was not implemented yet, but could be trivially added in the future. The `cutil_trace` introduces the concept of a tracebook, in order to be able to instrument codebase in multiple places at the same time. 

### Example 1: Adding multiple tracing points

Let's suppose we have a problem that `_all_docs` calls are not authorized properly for one specific user `foo`. We would like to know the value of `cors_config` field of `httpd` record passed to `chttpd_auth:authorize/2` and also log the `HandlerKey` for this request. 
In order to do that we want to trace following function. 

```
handle_req_after_auth(HandlerKey, HttpReq) ->
    #httpd{user_ctx = #user_ctx{name = User}} = HttpReq,
    ctrace:tag(#{user => User}),
    try
        HandlerFun = chttpd_handlers:url_handler(HandlerKey,
            fun chttpd_db:handle_request/1),
        AuthorizedReq = chttpd_auth:authorize(possibly_hack(HttpReq),
            fun chttpd_auth_request:authorize_request/1),
        {AuthorizedReq, HandlerFun(AuthorizedReq)}
    catch Tag:Error ->
        {HttpReq, catch_error(HttpReq, Tag, Error)}
    end.
```

In order to do that we would need to set the trigger and target. Trigger specify fir which function we should enable tracing. The target is a specification of what arguments to extract.
For our example we might specify the following trigger:

```
chttpd:handle_req_after_auth(_, #httpd{user_ctx = #user_ctx{name = <<"foo">>}}) ->
    ok.
```

The targets would be:
- `chttpd_auth:authorize(#httpd{cors_config = Config}, _) -> Config.`
- `chttpd_handlers:url_handler(HandlerKey, _) -> HandlerKey.`

The debugging session in this case would look like the following.

```
rr(httpd).
application:start(cutil).
TraceBook = {[
    {<<"id">>, <<"foo">>},
    {<<"points">>, [{[
        {<<"trigger">>, <<"
            chttpd:handle_req_after_auth(
                [_, #httpd{user_ctx = #user_ctx{name = <<\"foo\">>}}]) -> ok.
        ">>},
        {<<"targets">>, [
            <<"chttpd_auth:authorize([#httpd{cors_config = Config}, _]) -> Config.">>,
            <<"chttpd_handlers:url_handler([HandlerKey, _]) -> HandlerKey.">>
        ]}
    ]}]}
]},
TraceId = erlang:md5(term_to_binary(self())),
PoolPid = cutil_trace:get_pool(mypool, #{tracer => cutil_console_tracer}),
{#{points := Points}, []} = cutil_trace:parse_tracebook(TraceBook, fabric:records()),
cutil_trace:activate_points(Points, TraceId),
cutil_trace:trace(PoolPid, TraceId, filter).
```