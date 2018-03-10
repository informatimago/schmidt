//
//  UIViewExtensions.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 06/03/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit


extension UIView {

    func desktopView()->DesktopView? {
        var view:UIView?=self
        var desktopView=view as? DesktopView
        while (desktopView == nil) && (view != nil) {
            view=view!.superview
            desktopView=view as? DesktopView}
        return desktopView}

    func onDesktop()->Bool {
        return superview==desktopView()
    }

    func desktopToProcessTouches(_ touches:Set<UITouch>, with event: UIEvent?, fromView view:UIView) -> DesktopView? {
        if touches.count==1 {
            let touch=touches.first!
            let touchLocation=touch.location(in:view)
            let element=hitTest(touchLocation,with:event)
            if element is DesktopElement {
                if let desktop=desktopView() {
                    print("UIView extension: desktopToProcessTouches \(desktop) for element \(String(describing: element))")
                    return desktop }}}
        print("DesktopScrollView: desktopToProcessTouches nil")
        return nil
    }


}
