//
//  DesktopIcon.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopIcon: DesktopElement {

    var icon:UIImage?

    init(frame:CGRect,name:String,icon:UIImage){
        self.icon=icon
        super.init(frame:frame,name:name)
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

    override func draw(_ rect: CGRect) {
        icon?.draw(at:frame.origin)
        name.draw(in:frame)
        super.draw(rect)
    }

}
