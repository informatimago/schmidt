//
//  DesktopElement.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopElement: UIView {
    
    var name="Untitled"
    
    init(frame:CGRect,name:String){
        super.init(frame:frame)
        self.name=name
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

}
