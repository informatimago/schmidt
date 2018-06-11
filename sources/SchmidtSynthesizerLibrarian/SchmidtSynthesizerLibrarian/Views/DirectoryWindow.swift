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
    let scrollBorder:CGFloat=10.0

    init(frame:CGRect,directory:Directory<FileType>){
        self.directory=directory
        super.init(frame:frame,name:directory.name)
        let top:CGFloat=30
        let irect=CGRect(x:scrollBorder,y:top,width:frame.size.width-(2*scrollBorder),height:frame.size.height-top-scrollBorder)
        let scrollView=DesktopScrollView(frame:irect)
        scrollView.isScrollEnabled=true
        scrollView.isPagingEnabled=false
        scrollView.isDirectionalLockEnabled=true
        scrollView.bounces=true
        scrollView.alwaysBounceVertical=false
        scrollView.alwaysBounceHorizontal=false
        self.addSubview(scrollView)

        let root=DirectoryNode<FileType>(directory:directory)
        let treeView=TreeView(frame:CGRect(x:0,y:0,width:irect.width,height:irect.height),node:root)
        scrollView.addSubview(treeView)
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

}

