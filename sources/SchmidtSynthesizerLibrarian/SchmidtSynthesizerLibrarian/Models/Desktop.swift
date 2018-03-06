//
//  Desktop.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class Desktop: NSObject {

    var categories: [Category]=[]
    var elements:[NamedObject]=[]

    override init() {
        super.init()
        categories=defaultCategories()
        elements.append(programDirectory()!)
        elements.append(bankDirectory()!)
        elements.append(bankSetDirectory()!)
    }

    func defaultCategories() -> [Category] {
        var categories:[Category]=[]
        categories.append(Category(color:0,name:"Miscellaneous"))
        categories.append(Category(color:3,name:"Classic"))
        categories.append(Category(color:6,name:"Duo"))
        categories.append(Category(color:9,name:"Drone"))
        categories.append(Category(color:12,name:"Noise"))
        categories.append(Category(color:15,name:"Bass"))
        categories.append(Category(color:18,name:"Lead"))
        categories.append(Category(color:21,name:"Brass"))
        categories.append(Category(color:24,name:"WoodWind"))
        categories.append(Category(color:27,name:"Sound FX"))
        categories.append(Category(color:30,name:"Keys"))
        categories.append(Category(color:33,name:"Organs"))
        categories.append(Category(color:36,name:"Sync"))
        categories.append(Category(color:39,name:"Strings"))
        categories.append(Category(color:42,name:"Pads"))
        categories.append(Category(color:45,name:"Evolving"))
        categories.append(Category(color:48,name:"Distorted"))
        categories.append(Category(color:51,name:"Vox"))
        categories.append(Category(color:54,name:"Arp"))
        categories.append(Category(color:57,name:"Sequence"))
        categories.append(Category(color:60,name:"Percussion"))
        categories.append(Category(color:63,name:"User"))
        return categories
    }

    func dataDirectoryPath(name:String)->String?{
        let path=URL(fileURLWithPath:NSHomeDirectory(),isDirectory:true).appendingPathComponent("Documents/\(name)")
        do{
            try FileManager.default.createDirectory(at:path,
                                                withIntermediateDirectories:true,
                                                attributes:[:])
            return path.relativePath
        }catch let e {
            print("Cannot create directory \(path): \(e)")
            return nil
        }
    }

    func programDirectory()->Directory<Program>? {
        return Directory<Program>(path:dataDirectoryPath(name:"Programs")!)
    }

    func bankDirectory()->Directory<Bank>? {
        return Directory<Bank>(path:dataDirectoryPath(name:"Banks")!)
    }

    func bankSetDirectory()->Directory<BankSet>? {
        return Directory<BankSet>(path:dataDirectoryPath(name:"Sets")!)
    }

    func add(element:NamedObject){
        elements.append(element)
    }

    func remove(element:NamedObject){
        if let index=elements.index(of:element) {
            elements.remove(at:index)
        }
    }

}
