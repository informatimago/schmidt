//
//  DesktopViewController.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 15/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopViewController: UIViewController {

    @IBOutlet weak var desktopView: DesktopView!

    func typeLetter(_ instance:NamedObject) -> String {
        if(instance is Directory<Program>)  {return "DP"}
        if(instance is Directory<Bank>)     {return "DB"}
        if(instance is Directory<BankSet>)  {return "DS"}
        if(instance is Program)             {return "P"}
        if(instance is Bank)                {return "B"}
        if(instance is BankSet)             {return "S"}
        return "X"
    }

    var pos=10

    func add(instance:NamedObject){
        desktopView.add(element:DesktopInstance(frame:CGRect(x:10,y:pos,width:200,height:40),
                                                name:typeLetter(instance)+": "+instance.name,
                                                object:instance))
        pos+=40
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        let desktop=Desktop()
        let directory=desktop.programDirectory()!
        createTestDirectory(desktop)
        printDirectory(directory:directory,prefix:"  ")

        desktopView.desktop=desktop
        for instance in desktop.elements {
            add(instance:instance)
        }
        add(instance:Program(name:"PJB Test"))
        add(instance:Program(name:"ABCDEFGHIJKLMNOPQR"))
        add(instance:Bank(name:"Pads"))
        add(instance:BankSet(name:"Concert in Paris"))

        desktopView.add(element:DirectoryWindow<Program>(frame:CGRect(x:220,y:10,width:300,height:400),
                                                         directory:directory))
    }

    func printDirectory<FileType>(directory:Directory<FileType>,prefix:String){
        print("\(prefix) d \(directory.path)")
        for entry in directory.files() {
            print("\(prefix)   \(entry.path)")
        }
        for entry in directory.subdirectories() {
            printDirectory(directory:entry,prefix:"\(prefix)   ")
        }
    }


    func mkdir(_ path:String) throws {
        print("Create d \(path)")
        try FileManager.default.createDirectory(atPath:path,
                                            withIntermediateDirectories:true,
                                            attributes:[:])
    }

    func touch(_ path:String) throws {
        print("Create f \(path)")
        FileManager.default.createFile(atPath:path,contents:nil,attributes:[:])
    }

    func createTestDirectory(_ desktop:Desktop){
        let path=desktop.dataDirectoryPath(name:"Programs")!
        do {
            try mkdir(path)
            try touch(path.appending("/Foo.prg"))
            try touch(path.appending("/Bar.prg"))
            try mkdir(path.appending("/Basses"))
            try touch(path.appending("/Basses").appending("/Electric.prg"))
            try touch(path.appending("/Basses").appending("/Wet.prg"))
            try touch(path.appending("/Basses").appending("/Dry.prg"))
            try touch(path.appending("/Basses").appending("/Acoustic.prg"))
            try mkdir(path.appending("/Strings"))
            try touch(path.appending("/Strings").appending("/Violin.prg"))
            try touch(path.appending("/Strings").appending("/Cello.prg"))
            try touch(path.appending("/Strings").appending("/Viola.prg"))
            try touch(path.appending("/Strings").appending("/Contrebasse.prg"))
            print("**")
        }catch let e {
            print("CANNOT CREATE DIRECTORIES \(path): \(e)")
        }

    }
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
    }


}


