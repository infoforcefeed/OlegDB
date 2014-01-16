#pragma once

int ol_save_db(ol_database *db);
int ol_serialize_key_val(const char *key, ol_val);
int ol_load_db(char *filename);
