%%% Erlang record shared between many functions.
-record(ol_record, {database = <<"oleg">>,
                    expiration_time=-1,
                    key, % Binary
                    content_type = <<"application/octet-stream">>,
                    value = <<>>,
                    content_length=0}).
