//
//  DesktopElement.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopElement: UIView
// ,UIDragInteractionDelegate,NSItemProviderWriting
{
    
    var name="Untitled"
    
    init(frame:CGRect,name:String){
        super.init(frame:frame)
        self.name=name
        self.isUserInteractionEnabled=true
   }

    required init?(coder:NSCoder){
        super.init(coder:coder)
        self.isUserInteractionEnabled=true
   }


    var selected=false
    var dragging=false

    override func draw(_ rect: CGRect) {
        if selected || dragging {
            let color:UIColor=UIColor.black
            let path:UIBezierPath=UIBezierPath(rect:bounds)
            color.set()
            path.lineWidth=3.0
            path.stroke()
        }
    }

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?){
        dragging=true
        superview!.bringSubview(toFront:self)
        setNeedsDisplay()
    }

    func pointMinus(_ a:CGPoint,_ b:CGPoint) -> CGPoint{
        return CGPoint(x:a.x-b.x,y:a.y-b.y)
    }

    func pointAdd(_ a:CGPoint,_ b:CGPoint) -> CGPoint{
        return CGPoint(x:a.x+b.x,y:a.y+b.y)
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
        dragging=false
        setNeedsDisplay()
    }

    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?){
        dragging=false
        setNeedsDisplay()
    }


}
