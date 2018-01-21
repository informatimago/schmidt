//
//  DirectoryWindow.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 20/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DirectoryWindow<FileType>: DesktopWindow where FileType:NamedObject {

    var directory:Directory<FileType>?

    init(frame:CGRect,directory:Directory<FileType>){
        self.directory=directory
        super.init(frame:frame,name:directory.name)

        let irect=CGRect(x:5,y:30,width:frame.size.width-10,height:frame.size.height-30)
        let scrollView=UIScrollView(frame:irect)
        self.addSubview(scrollView)

//        let trect=CGRect(x:0,y:0,width:frame.size.width-10,height:frame.size.height-30)
//        let tree=RATreeView.init(frame:trect)
//        scrollView.addSubview(tree)

//
//        var i=1
//        for program in bank.programs {
//            self.addSubview(DesktopInstance(frame:irect,name:String(i)+": "+program.name,object:program))
//            irect.origin.y+=20
//            i+=1
//        }
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

}

