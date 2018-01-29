//
//  TreeView.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 21/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import Foundation
import UIKit

// Node and Leaf are the protocol implemented by the client data.
// Those protocol include a view() method to obtain the UIView used to display the Node and Leaf data.
// Those views will be embedded in NodeView to position and decorate them with the needed controls.

protocol Leaf {
    func view()->UIView
}

protocol Node {
    func view()->UIView
    func leaves()->[Leaf]
    func subtrees()->[Node]
}


// TreeViewElement is the common interface (protocol) of the TreeViewLeaf and TreeViewSubtree classes.
// The TreeViewElement classes will parallel the tree described by Node and Leaf, in a lazy way:
// only the TreeViewSubtree nodes that are "open" will load the leaves and subtrees from the Node.

protocol TreeViewElement {
    func isLeaf()->Bool
    func leaf()->Leaf?
    func node()->Node?
    func subleaves()->[TreeViewLeaf]
    func subtrees()->[TreeViewSubtree]
    func isOpen()->Bool
    func open()
    func close()
}

class TreeViewLeaf:TreeViewElement {
    var _leaf:Leaf
    init(leaf:Leaf)                    { _leaf=leaf }
    func isLeaf()->Bool                { return true }
    func leaf()->Leaf?                 { return _leaf }
    func node()->Node?                 { return nil }
    func subleaves()->[TreeViewLeaf]   { return [] }
    func subtrees()->[TreeViewSubtree] { return [] }
    func isOpen()->Bool                { return false }
    func open()                        {}
    func close()                       {}
}

class TreeViewSubtree:TreeViewElement {
    var _node:Node
    var _open:Bool=false
    var _subtrees:[TreeViewSubtree]=[]
    var _leaves:[TreeViewLeaf]=[]
    init(node:Node)                    { _node=node   }
    func isLeaf()->Bool                { return false }
    func leaf()->Leaf?                 { return nil   }
    func node()->Node?                 { return _node }
    func subleaves()->[TreeViewLeaf]   { return lazyComputeSubleaves() }
    func subtrees()->[TreeViewSubtree] { return lazyComputeSubtrees()  }
    func isOpen()->Bool                { return _open }
    func open()                        { _open=true   }
    func close()                       { _open=false  }

    func lazyComputeSubleaves()->[TreeViewLeaf]{
        if _node.leaves().count>0 && _leaves.count==0 {
            for leaf in _node.leaves() {
                _leaves.append(TreeViewLeaf(leaf:leaf))
            }
        }
        return _leaves
    }

    func lazyComputeSubtrees()->[TreeViewSubtree]{
        if _node.subtrees().count>0 && _subtrees.count==0 {
            for subtree in _node.subtrees() {
                _subtrees.append(TreeViewSubtree(node:subtree))
            }
        }
        return _subtrees
    }

}

class TreeView:UIView {

    var indent:CGFloat=32.0
    var subtree:TreeViewSubtree?=nil
    var nodeView:UIView?=nil

    init(frame:CGRect,node:Node) {
        self.subtree=TreeViewSubtree(node:node)
        self.nodeView=node.view()
        self.nodeView?.frame.origin=CGPoint(x:indent,y:0)
        super.init(frame:CGRect(x:frame.origin.x,
                                y:frame.origin.y,
                                width:indent+self.nodeView!.frame.size.width,
                                height:self.nodeView!.frame.size.height))
        self.backgroundColor=UIColor.white
        self.addSubview(nodeView!)
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder:aDecoder)
    }

    override func draw(_ rect:CGRect){
        super.draw(rect)
        let color=UIColor.black
        let path=UIBezierPath()
        let center=CGPoint(x:self.bounds.origin.x+indent/2,
                           y:self.bounds.origin.y+nodeView!.frame.size.height/2)
        if subtree!.isOpen() {
            //   \/
            path.move(to:CGPoint(x:center.x-5,y:center.y-4))
            path.addLine(to:CGPoint(x:center.x,y:center.y+4))
            path.addLine(to:CGPoint(x:center.x+5,y:center.y-4))
        }else{
            //    >
            path.move(to:CGPoint(x:center.x-4,y:center.y+5))
            path.addLine(to:CGPoint(x:center.x+4,y:center.y))
            path.addLine(to:CGPoint(x:center.x-4,y:center.y-5))
        }
        color.set()
        path.close()
        path.stroke()
    }
    
    func createSubviews(x:CGFloat,y:CGFloat) -> CGSize {
        var newWidth=x+frame.size.width
        var newHeight=y+frame.size.height
        for subleaf in subtree!.subleaves() {
            let subview=subleaf.node()!.view()
            subview.frame.origin=CGPoint(x:indent,y:newHeight)
            addSubview(subview)
            newWidth=max(newWidth,subview.frame.size.width)
            newHeight=newHeight+subview.frame.size.height
        }
        for subnode in subtree!.subtrees() {
            let treeView=TreeView(frame:CGRect(x:indent,y:newHeight,width:frame.size.width,height:frame.size.height),
                                  node:subnode.node()!)
            addSubview(treeView)
            newWidth=max(newWidth,treeView.frame.size.width)
            newHeight=newHeight+treeView.frame.size.height
        }
        return CGSize(width:newWidth,height:newHeight)
    }


    func switchOpening() {
        if subtree!.isOpen(){
            subtree!.close()
            for subview in self.subviews {
                if subview != nodeView {
                    subview.removeFromSuperview()
                }
            }
            self.frame=CGRect(origin:frame.origin,
                              size:CGSize(width:frame.size.width,
                                          height:nodeView!.frame.size.height))
        }else{
            subtree!.open()
            self.frame=CGRect(origin:frame.origin,
                              size:createSubviews(x:indent,y:nodeView!.frame.size.height))
        }
        setNeedsDisplay()
    }

    func openingArea()->CGRect {
        return CGRect(origin:bounds.origin,size:CGSize(width:indent,height:nodeView!.frame.size.height))
    }

    func inOpeningArea(touch:UITouch) -> Bool {
        return openingArea().contains(touch.location(in:self))
    }

    var beganInOpeningArea=false

    override func touchesBegan(_ touches: Set<UITouch>,with event: UIEvent?){
        if touches.count == 1 {
            beganInOpeningArea=inOpeningArea(touch:touches.first!)
        }
    }

    override func touchesEnded(_ touches: Set<UITouch>,with event: UIEvent?){
        if touches.count == 1 {
            if beganInOpeningArea && inOpeningArea(touch:touches.first!) {
                switchOpening()
            }
        }

    }

}

