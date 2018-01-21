//
//  TreeView.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 21/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import Foundation
import UIKit

protocol Leaf {
    func view()->UIView
}

protocol Node {
    func view()->UIView
    func leaves()->[Leaf]
    func subtrees()->[Node]
}

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

    var root:TreeViewSubtree?
    var indent:CGFloat=20.0

    init(frame:CGRect,root:Node) {
        self.root=TreeViewSubtree(node:root)
        super.init(frame:frame)
        self.frame=CGRect(origin:frame.origin,size:createSubviews(element:self.root!,x:0.0,y:0.0))
    }

    required init?(coder aDecoder: NSCoder) {
        super.init(coder:aDecoder)
    }

    func createSubviews(element:TreeViewElement,x:CGFloat,y:CGFloat) -> CGSize {
        var subview:UIView
        if element.isLeaf() {
            subview=element.leaf()!.view()
        }else{
            subview=element.node()!.view()
        }
        subview.frame=CGRect(origin:CGPoint(x:x,y:y),size:subview.frame.size)
        self.addSubview(subview)
        var newWidth=x+subview.frame.size.width
        var newHeight=y+subview.frame.size.height
        if element.isOpen() {
            for subtree in element.subtrees() {
                let subSize=createSubviews(element:subtree,x:x+indent,y:newHeight)
                newWidth=max(newWidth,subSize.width)
                newHeight=newHeight+subSize.height
            }
            for subleaf in element.subleaves() {
                let subSize=createSubviews(element:subleaf,x:x+indent,y:newHeight)
                newWidth=max(newWidth,subSize.width)
                newHeight=newHeight+subSize.height
            }
        }
        return CGSize(width:newWidth,height:newHeight)
    }


    func switchOpening() {

    }

    func openingArea()->CGRect {
        return CGRect(origin:bounds.origin,size:CGSize(width:indent,height:bounds.size.height))
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

