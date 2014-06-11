%%% Erlang record shared between many functions.
-define(DEFAULT_DBNAME, <<"oleg">>).
-record(ol_record, {database = ?DEFAULT_DBNAME,
                    expiration_time=-1,
                    key, % Binary
                    content_type = <<"application/octet-stream">>,
                    value = <<>>,
                    content_length=0}).
