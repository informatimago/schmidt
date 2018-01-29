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
    var onDesktop=true

    init(frame:CGRect,name:String,object:NamedObject){
        self.object=object
        super.init(frame:frame,name:name)
        self.backgroundColor=UIColor.white
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
        self.backgroundColor=UIColor.white
   }


    override func draw(_ rect: CGRect) {
        let color:UIColor=UIColor.black
        let path:UIBezierPath=UIBezierPath(rect:bounds)
        color.set()
        path.stroke()
        name.draw(in:bounds.insetBy(dx:4,dy:4))
        // draw type icon
        super.draw(rect)
    }




    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?){
        super.touchesBegan(touches,with:event)
    }

    override func touchesMoved(_ touches: Set<UITouch>,with event: UIEvent?){
        if touches.count==1 {
            let touch=touches.first!
            let touchLocation=touch.location(in:self)
            if window!.frame.contains(touchLocation) {
                let newOrigin=pointAdd(frame.origin,pointMinus(touchLocation,touch.previousLocation(in:self)))
                frame=CGRect(origin:newOrigin,size:frame.size)
            }
        }
    }

    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?){
        if onDesktop {
            super.touchesEnded(touches,with:event)
        }else{
            touchesCancelled(touches, with: event)
        }
    }

}
