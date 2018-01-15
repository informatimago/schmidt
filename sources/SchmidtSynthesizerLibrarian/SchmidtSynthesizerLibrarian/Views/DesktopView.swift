//
//  DesktopView.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopView: UIView {
    
    var desktop=Desktop()
    var elements:[DesktopElement]=[]
    var selectedElements:[DesktopElement]=[]
    
    override init(frame:CGRect){
        super.init(frame:frame)
    }
    
    required init?(coder:NSCoder){
        super.init(coder:coder)
    }
    
}
