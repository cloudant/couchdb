defprotocol Couch.Test.Adapter do
  def login(adapter, user, pass)
  def create_user(adapter, user \\ [])
  def create_user_from_doc(adapter, user_doc)
  def create_db(adapter, db_name, opts \\ [])
  def delete_db(adapter, db_name)
  def create_doc(adapter, db_name, body)
  def bulk_save(adapter, db_name, docs)  
  def query(
        adapter,
        db_name,
        map_fun,
        reduce_fun \\ nil,
        options \\ nil,
        keys \\ nil,
        language \\ "javascript"
      )
  def set_config(adapter, section, key, val)
  def add_admin(adapter, admin_name, pass)
      
end
