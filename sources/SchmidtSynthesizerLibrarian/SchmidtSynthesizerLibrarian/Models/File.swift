//
//  File.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 20/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class File<FileType>: DirectoryEntry<FileType> where FileType:NamedObject {

    override func isFile() -> Bool     { return true }

    func contents() -> FileType {
        return FileType.init(name:self.name)
    }

}

