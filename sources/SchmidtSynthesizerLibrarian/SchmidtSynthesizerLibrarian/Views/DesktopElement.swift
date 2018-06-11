//
//  DesktopElement.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

func pointMinus(_ a:CGPoint,_ b:CGPoint) -> CGPoint{
    return CGPoint(x:a.x-b.x,y:a.y-b.y)}

func pointAdd(_ a:CGPoint,_ b:CGPoint) -> CGPoint{
    return CGPoint(x:a.x+b.x,y:a.y+b.y)}



class DesktopElement: UIView,DropTarget
// ,UIDragInteractionDelegate,NSItemProviderWriting
{
    
    var name="Untitled"

    init(frame:CGRect,name:String){
        super.init(frame:frame)
        self.name=name
        self.isUserInteractionEnabled=true
        self.backgroundColor=UIColor.white}

    required init?(coder:NSCoder){
        super.init(coder:coder)
        self.isUserInteractionEnabled=true}


    var selected=false
    var dragging=false

    override func draw(_ rect: CGRect) {
        if selected || dragging {
            frameRect(bounds,color:UIColor.black,borderWidth:3) }}

    func image()->UIImage {
        UIGraphicsBeginImageContext(bounds.size)
        let context = UIGraphicsGetCurrentContext()!
        context.translateBy(x:-bounds.origin.x,y:-bounds.origin.y)
        dragging=true
        fillRect(bounds,color:UIColor.white)
        draw(bounds)
        dragging=false
        let image = UIGraphicsGetImageFromCurrentImageContext()!
        UIGraphicsEndImageContext()
        return image}

    func imageView()->UIImageView {
        let view=UIImageView(frame:frame)
        view.image=image()
        return view}

    func dragAndDrop(fromOffset:CGPoint,to:CGPoint,onDesktop:DesktopView){
        if(superview==onDesktop){
            frame.origin=pointMinus(to,fromOffset)
            setNeedsDisplay()
        // else subclass responsibility
        }}

    func dragAndDrop(fromOffset:CGPoint,to:CGPoint,onElement:DesktopElement){
        // subclass responsibility
    }


    // Drop Target:
    func canTake(element:DesktopElement,from:CGPoint,to:CGPoint)->Bool {return false}
    func startDrop(element:DesktopElement,from:CGPoint,to:CGPoint)     {}
    func cancelDrop(element:DesktopElement,from:CGPoint)               {}
    func completeDrop(element:DesktopElement,from:CGPoint,to:CGPoint)  {}

    // Dropping elements:
    func canDrop(on:DropTarget,from:CGPoint,to:CGPoint) -> Bool        {return false}

    func open(){
        // subclass responsibility;
    }
    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?){
        if !dragging && (touches.count==2) {
            self.open();
        }else{
            super.touchesEnded(touches,with:event);
        }
    }
}
