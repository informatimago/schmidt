//
//  DesktopScrollView.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 06/03/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit


class DesktopScrollView: UIScrollView {

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?){
        if let desktop=desktopToProcessTouches(touches,with:event,fromView:self) {
            desktop.touchesBegan(touches,with:event,fromView:self)
        }else{
            super.touchesBegan(touches,with:event)
        }
    }

    override func touchesMoved(_ touches: Set<UITouch>,with event: UIEvent?){
        if let desktop=desktopToProcessTouches(touches,with:event,fromView:self) {
            desktop.touchesMoved(touches,with:event,fromView:self)
        }else{
            super.touchesMoved(touches,with:event)
        }
    }

    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?){
        if let desktop=desktopToProcessTouches(touches,with:event,fromView:self) {
            desktop.touchesEnded(touches,with:event,fromView:self)
        }else{
            super.touchesEnded(touches,with:event)
        }
    }

    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?){
        if let desktop=desktopToProcessTouches(touches,with:event,fromView:self) {
            desktop.touchesCancelled(touches,with:event,fromView:self)
        }else{
            super.touchesCancelled(touches,with:event)
        }
    }

}
