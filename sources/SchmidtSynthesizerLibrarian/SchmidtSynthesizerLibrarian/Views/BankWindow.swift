//
//  BankWindow.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class BankWindow: DesktopWindow {

    var bank:Bank?

    init(frame:CGRect,name:String,bank:Bank){
        super.init(frame:frame,name:name)
        self.bank=bank

        // Add subviews for programs
        var irect=CGRect(x:10,y:30,width:frame.size.width-20,height:20)
        var i=1
        for program in bank.programs {
            self.addSubview(DesktopInstance(frame:irect,name:String(i)+": "+program.name,object:program))
            irect.origin.y+=20
            i+=1
        }
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

}
