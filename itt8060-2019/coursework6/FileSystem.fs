module FileSystem

    type FsTree = Node of (string * FsTree) list


    let show (fs : FsTree) : string list list =
        failwith "not implemented"

    let createDir (p : string list) (fs : FsTree) : FsTree =
        failwith "not implemented"

    let delete (p : string list) (fs : FsTree) : FsTree =
        failwith "not implemented"
