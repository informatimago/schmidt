//
//  Directory.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 20/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class Directory<FileType>: DirectoryEntry<FileType>  where FileType:NamedObject {

    override func isDirectory() -> Bool { return true }

    func filteredEntries(includeFiles:Bool,includeDirectories:Bool)->[DirectoryEntry<FileType>]{
        do {
            var result:[DirectoryEntry<FileType>]=[]
            for subpath in try FileManager.default.contentsOfDirectory(atPath:path){
                var isDirectory=ObjCBool(false)
                let exists=FileManager.default.fileExists(atPath:path.appending("/"+subpath),
                                                          isDirectory: UnsafeMutablePointer<ObjCBool>(&isDirectory))
                print("subpath \(subpath) exists? \(exists)")
                if exists {
                    if isDirectory.boolValue {
                        print("dire \(URL(fileURLWithPath:path,isDirectory:true).appendingPathComponent(subpath).path)")
                        result.append(Directory(path:URL(fileURLWithPath:path,isDirectory:true).appendingPathComponent(subpath).path))
                    }else{
                        print("file \(URL(fileURLWithPath:path,isDirectory:true).appendingPathComponent(subpath).path)")
                        result.append(File(path:URL(fileURLWithPath:path,isDirectory:true).appendingPathComponent(subpath).path))
                    }
                }
            }
            for entry in result {
                print("result \(type(of:entry)) \(entry.path)")
            }
            return result
        }catch let e {
            print("ERROR: \(e)")
            return []
        }
    }

    override func entries()->[DirectoryEntry<FileType>]{
        return filteredEntries(includeFiles:true,includeDirectories:true)
    }

    override func files()->[File<FileType>]{
        var result:[File<FileType>]=[]
        for entry in filteredEntries(includeFiles:true,includeDirectories:false){
            if entry is File<FileType> {
                result.append(entry as! File<FileType>)
            }
        }
        return result
    }

    override func subdirectories()->[Directory<FileType>]{
        var result:[Directory<FileType>]=[]
        for entry in filteredEntries(includeFiles:false,includeDirectories:true){
            if entry is Directory<FileType> {
                result.append(entry as! Directory<FileType>)
            }
        }
        return result
    }

}
