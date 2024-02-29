#!/usr/bin/env python3

import sys
from pathlib import Path
import google.protobuf.text_format as text_format
import google.protobuf.json_format as json_format
import metadata_source_pb2 as mds

def qc_message_from_text(qc_text_path):
    qc_text_string = qc_text_path.read_text()
    mds_qc = mds.MetadataSourceQueryConfig()
    text_format.Parse(qc_text_string, mds_qc)
    return mds_qc

def make_merged_qc(mds_base_path, mds_sqlite_path):
    mds_qc = qc_message_from_text(Path(mds_base_path))
    mds_sqlite_qc = qc_message_from_text(Path(mds_sqlite_path))
    mds_qc.MergeFrom(mds_sqlite_qc)
    return mds_qc

def usage():
    print("Usage: dump_qc_proto.py <base_query_config.proto> <sqlite_query_config.proto> <merged_query_config.json>")

if __name__ == '__main__':
    if len(sys.argv) != 4:
        usage()
        sys.exit(1)
    # ensure both files exist
    for qc_text_path in sys.argv[1:3]:
        if not Path(qc_text_path).exists():
            print(f"File {qc_text_path} does not exist")
            usage()
            sys.exit(1)

    mds_base_path = Path(sys.argv[1])
    mds_sqlite_path = Path(sys.argv[2])
    merged_path = Path(sys.argv[3])
    merged_qc = make_merged_qc(mds_base_path, mds_sqlite_path)

    with open(merged_path, "w") as f:
        print(f"Writing merged query config to {merged_path}")
        f.write(json_format.MessageToJson(merged_qc))
