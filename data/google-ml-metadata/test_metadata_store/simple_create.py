#!/usr/bin/env python3

from ml_metadata import metadata_store
from ml_metadata.proto import metadata_store_pb2

def create_connection_config():
    connection_config = metadata_store_pb2.ConnectionConfig()
    connection_config.sqlite.filename_uri = 'ml_metadata.sqlite'
    connection_config.sqlite.connection_mode = 3 # READWRITE_OPENCREATE
    return connection_config

def create_metadata_store(connection_config):
    store = metadata_store.MetadataStore(connection_config)
    return store

if __name__ == '__main__':
    connection_config = create_connection_config()
    store = create_metadata_store(connection_config)
    print(store)
