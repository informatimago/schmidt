//
//  DesktopView.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

protocol DropTarget {
    func canTake(element:DesktopElement,from:CGPoint,to:CGPoint)->Bool
    func startDrop(element:DesktopElement,from:CGPoint,to:CGPoint)
    func completeDrop(element:DesktopElement,from:CGPoint,to:CGPoint)
    func cancelDrop(element:DesktopElement,from:CGPoint)
}

func frameRect(_ rect:CGRect,color:UIColor,borderWidth:CGFloat){
    let path:UIBezierPath=UIBezierPath(rect:rect)
    path.lineWidth=borderWidth
    color.set()
    path.stroke() }

func fillRect(_ rect:CGRect,color:UIColor){
    let path:UIBezierPath=UIBezierPath(rect:rect)
    color.set()
    path.fill() }

class DesktopView: UIView,DropTarget {

    var desktop:Desktop?
    var elements:[DesktopElement]=[]
    var selectedElements:[DesktopElement]=[]
    var isDropDestination=false

    override init(frame:CGRect){
        super.init(frame:frame) }

    required init?(coder:NSCoder){
        super.init(coder:coder) }

    func add(element:DesktopElement){
        elements.append(element)
        addSubview(element) }

    func remove(element:DesktopElement){
        element.removeFromSuperview()
        if let index=elements.index(of:element) {
            elements.remove(at:index) }}

    override func draw(_ rect:CGRect){
        super.draw(rect)
        if isDropDestination {
            frameRect(bounds,color:UIColor.yellow,borderWidth:5) }}

    // Drop Target:

    func canTake(element:DesktopElement,from:CGPoint,to:CGPoint)->Bool {
        var result=false
        print("canTake onDesktop:\(element.onDesktop()) hitTest:\(String(describing: hitTest(to,with:nil)))")
        if element.onDesktop() {
            result=(self==hitTest(to,with:nil))
        }else{
            result=element.canDrop(on:self,from:from,to:to) }
        print("canTake element:\(element) from:\(from) to:\(to) -> \(result)")
        return result
    }

    func startDrop(element:DesktopElement,from:CGPoint,to:CGPoint){
        print("startDrop element:\(element) from:\(from) to:\(to)")
        isDropDestination=true
        setNeedsDisplay() }

    func completeDrop(element:DesktopElement,from:CGPoint,to:CGPoint){
        print("completeDrop element:\(element) from:\(from) to:\(to)")
        isDropDestination=false
        element.frame=CGRect(origin:pointAdd(element.frame.origin,pointMinus(to,from)),
                             size:element.frame.size)
        setNeedsDisplay() }

    func cancelDrop(element:DesktopElement,from:CGPoint){
        print("cancelDrop element:\(element) from:\(from)")
        isDropDestination=false
        setNeedsDisplay() }

    // Drag-and-Drop:

    var dragging=false
    var startLocation=CGPoint(x:0,y:0)                 // self coordinate system
    var originalFrame=CGRect(x:0,y:0,width:0,height:0) // self coordinate system
    var currentElement:DesktopElement?
    var currentImageView:UIImageView?
    var currentTarget:DropTarget?

    func startDragging(element:DesktopElement,fromLocation:CGPoint){
        print("startDragging element \(element) fromLocation \(fromLocation)")
        dragging=true
        startLocation=fromLocation
        originalFrame=element.superview!.convert(element.frame,to:self)
        currentElement=element
        currentImageView=element.imageView()
        addSubview(currentImageView!)
        setNeedsDisplay() }

    func dragImage(toLocation:CGPoint){
        let newOrigin=pointAdd(originalFrame.origin,pointMinus(toLocation,startLocation))
        let newFrame=CGRect(origin:newOrigin,size:originalFrame.size)
        currentImageView!.frame=newFrame
        setNeedsDisplay() }

    func drop(element:DesktopElement,to:CGPoint,on:UIView){
        if let desktopView=on as? DesktopView {
            element.dragAndDrop(fromOffset:convert(startLocation,to:element),
                                to:convert(to,to:desktopView),
                                onDesktop:desktopView)
        }else if let targetElement=on as? DesktopElement {
            element.dragAndDrop(fromOffset:convert(startLocation,to:element),
                                to:convert(to,to:targetElement),
                                onElement:targetElement) }}

    func finishDragging(){
        dragging=false
        if currentImageView != nil {
            currentImageView!.removeFromSuperview() }
        currentImageView=nil
        setNeedsDisplay() }

    // Events:

    func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?, fromView view:UIView){
        if touches.count==1 {
            let touch=touches.first!
            let touchLocation=touch.location(in:view)
            let element=view.hitTest(touchLocation,with:event)
            print("DesktopView touchesBegan element = \(String(describing: element))")
            if let element = element as? DesktopElement {
                startDragging(element:element ,fromLocation:convert(touchLocation,from:view)) }}}

    func updateTarget(touchLocation:CGPoint,with event:UIEvent?,update:(DropTarget,CGPoint)->Void){
        var newTarget=hitTest(touchLocation,with:event) as? DropTarget
        if newTarget == nil {
            newTarget=self }
        let new=newTarget as! UIView
        if currentTarget != nil {
            let current=currentTarget as! UIView
            if (current != new) {
                if (currentTarget != nil) {
                    currentTarget!.cancelDrop(element:currentElement!,from:startLocation)
                    currentTarget=nil }}}
        if newTarget!.canTake(element:currentElement!,from:startLocation,to:touchLocation) {
            update(newTarget!,touchLocation) }}

    func touchesMoved(_ touches: Set<UITouch>,with event: UIEvent?, fromView view:UIView){
        if dragging && (touches.count==1) {
            let touch=touches.first!
            let touchLocation=touch.location(in:view)
            if window!.frame.contains(convert(touchLocation,from:view)) {
                dragImage(toLocation:convert(touchLocation,from:view))
                updateTarget(touchLocation:convert(touchLocation,from:view),
                             with:event,
                             update:{ (_ newTarget:DropTarget,_ touchLocation:CGPoint) in
                                        newTarget.startDrop(element:currentElement!,from:startLocation,to:touchLocation)
                                        currentTarget=newTarget }) }}}

    func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?, fromView view:UIView){
        if dragging && (touches.count==1) {
            let touch=touches.first!
            let touchLocation=touch.location(in:view)
            if window!.frame.contains(convert(touchLocation,from:view)) {
                dragImage(toLocation:convert(touchLocation,from:view))
                updateTarget(touchLocation:convert(touchLocation,from:view),
                             with:event,
                             update:{ (_ newTarget:DropTarget,_ touchLocation:CGPoint) in
                                newTarget.completeDrop(element:currentElement!,from:startLocation,to:touchLocation) }) }}
        finishDragging() }

    func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?,fromView view:UIView){
        if (currentTarget != nil) {
            currentTarget!.cancelDrop(element:currentElement!,from:startLocation)
            currentTarget=nil }
        finishDragging() }

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?){
        touchesBegan(touches,with:event,fromView:self) }
    override func touchesMoved(_ touches: Set<UITouch>,with event: UIEvent?){
        touchesMoved(touches,with:event,fromView:self) }
    override func touchesEnded(_ touches: Set<UITouch>,with event: UIEvent?){
        touchesEnded(touches,with:event,fromView:self) }
    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?){
        touchesCancelled(touches,with:event,fromView:self) }

}
