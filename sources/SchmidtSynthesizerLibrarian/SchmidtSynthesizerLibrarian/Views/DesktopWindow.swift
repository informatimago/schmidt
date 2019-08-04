//
//  DesktopWindow.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopWindow: DesktopElement {

    var contents:UIView?

    override init(frame:CGRect,name:String){
        super.init(frame:frame,name:name)
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

    override func draw(_ rect:CGRect) {
        drawWindowFrame()
        super.draw(rect) // draw contents
    }

    func drawWindowFrame(){
        let color:UIColor=UIColor.black
        let path:UIBezierPath=UIBezierPath(rect:bounds)
        color.set()
        path.stroke()
        name.draw(in:bounds)
    }

}
