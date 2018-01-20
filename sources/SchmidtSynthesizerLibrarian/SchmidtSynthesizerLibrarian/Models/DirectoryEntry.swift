//
//  DirectoryEntry.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 20/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DirectoryEntry<FileType>: NamedObject where FileType:NamedObject {

    var path:String

    required init(name:String){
        self.path=name
        super.init(name:name)
    }

    init(path:String){
        self.path=path
        super.init(name:URL(fileURLWithPath:self.path).lastPathComponent)
    }

    func isFile()         -> Bool                       { return false }
    func isDirectory()    -> Bool                       { return false }
    func entries()        -> [DirectoryEntry<FileType>] { return [] }
    func files()          -> [File<FileType>]           { return [] }
    func subdirectories() -> [Directory<FileType>]      { return [] }

}
