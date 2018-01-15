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
        self.backgroundColor=UIColor.white
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

    override func draw(_ rect: CGRect) {
        let color:UIColor=UIColor.black
        let path:UIBezierPath=UIBezierPath(rect:bounds)
        color.set()
        path.stroke()
        name.draw(in:bounds.insetBy(dx:4,dy:4))
        // draw type icon
    }
}
