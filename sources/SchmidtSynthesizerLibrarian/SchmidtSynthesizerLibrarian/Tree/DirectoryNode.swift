//
//  DirectoryNode.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 29/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

func typeLetter(_ instance:NamedObject) -> String {
    if(instance is Directory<Program>)  {return "DP"}
    if(instance is Directory<Bank>)     {return "DB"}
    if(instance is Directory<BankSet>)  {return "DS"}
    if(instance is File<Program>)       {return "FP"}
    if(instance is File<Bank>)          {return "FB"}
    if(instance is File<BankSet>)       {return "FS"}
    if(instance is Program)             {return "P"}
    if(instance is Bank)                {return "B"}
    if(instance is BankSet)             {return "S"}
    return "X"
}

func desktopInstance(instance:NamedObject) -> DesktopInstance {
    return DesktopInstance(frame:CGRect(x:0,y:0,width:200,height:40),
                           name:typeLetter(instance)+": "+instance.name,
                           object:instance)
}

class DirectoryLeaf<FileType>:Leaf where FileType:NamedObject {

    var file:File<FileType>

    init(file:File<FileType>){
        self.file=file
    }

    func name() -> String {
        return file.name
    }

    func view()->UIView {
        return desktopInstance(instance:file)
    }

}

class DirectoryNode<FileType>:Node where FileType:NamedObject {

    var directory:Directory<FileType>

    init(directory:Directory<FileType>){
        self.directory=directory
    }

    func name() -> String {
        return directory.name
    }

    func view()->UIView{
        return desktopInstance(instance:directory)
    }

    func leaves()->[Leaf]{
        var result:[Leaf]=[]
        for file in directory.files() {
            result.append(DirectoryLeaf<FileType>(file:file))
        }
        return result
    }

    func subtrees()->[Node]{
        var result:[Node]=[]
        for directory in directory.subdirectories() {
            result.append(DirectoryNode<FileType>(directory:directory))
        }
        return result
    }
}
