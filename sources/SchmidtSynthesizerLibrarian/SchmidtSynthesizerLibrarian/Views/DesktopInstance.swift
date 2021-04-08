//
//  DesktopInstance.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopInstance: DesktopElement {

    var object:NamedObject?

    init(frame:CGRect,name:String,object:NamedObject){
        self.object=object
        super.init(frame:frame,name:name)
        self.backgroundColor=UIColor.white }

    required init?(coder:NSCoder){
        super.init(coder:coder)
        self.backgroundColor=UIColor.white }

    override func draw(_ rect: CGRect) {
        let color:UIColor=UIColor.black
        let path:UIBezierPath=UIBezierPath(rect:bounds)
        color.set()
        path.stroke()
        name.draw(in:bounds.insetBy(dx:4,dy:4))
        // draw type icon
        super.draw(rect) }


    // Drop Target:
    override func canTake(element:DesktopElement,from:CGPoint,to:CGPoint)->Bool {return false}
    override func startDrop(element:DesktopElement,from:CGPoint,to:CGPoint)     {}
    override func cancelDrop(element:DesktopElement,from:CGPoint)               {}
    override func completeDrop(element:DesktopElement,from:CGPoint,to:CGPoint)  {}

    // Dropping elements:
    override func canDrop(on:DropTarget,from:CGPoint,to:CGPoint) -> Bool        {return false}

    override func open(){
        desktopView()?.add(element:DesktopIcon(frame:CGRect(x:50,y:40,width:300,height:200),
            name:"SchmidtSynthesizer",
            icon:(UIImage(contentsOfFile:"schmidt_detail_01")!)));
    }


/*

                                     completeDrop                       startDrop

     file -> directory               move             (option)copy

     file      -> desktop            alias
     directory -> desktop            alias

     program file -> bank file       insert program                     (select program slot in bank file)
     program file -> bank            insert program                     (select program slot in bank)
     program      -> bank            insert program                     (select program slot in bank)

     bank file -> bank set file      insert bank                        (select bank slot in bank set file)
     bank file -> bank set           insert bank                        (select bank slot in bank set)
     bank      -> bank set           insert bank                        (select bank slot in bank set)

     program  -> program directory   save data to file                  (select subdirectory)
     bank     -> bank directory      save data to file                  (select subdirectory)
     bank set -> bank set directory  save data to file                  (select subdirectory)

     file     -> trash               delete file
     alias    -> trash               remove from desktop
     item     -> trash               remove from desktop

     category -> program             set program category
     program  -> category            set program category

 */

}
