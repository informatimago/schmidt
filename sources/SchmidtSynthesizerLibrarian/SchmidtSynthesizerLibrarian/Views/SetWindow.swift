//
//  SetWindow.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class SetWindow: DesktopWindow {

    var set:Set?
    
    init(frame:CGRect,name:String,set:Set){
        super.init(frame:frame,name:name)
        self.set=set

        var irect=CGRect(x:10,y:30,width:frame.size.width-20,height:20)
        var i=1
        for bank in set.banks {
            self.addSubview(DesktopInstance(frame:irect,name:String(i)+": "+bank.name,object:bank))
            irect.origin.y+=20
            i+=1
       }
    }

    required init?(coder:NSCoder){
        super.init(coder:coder)
    }

}
