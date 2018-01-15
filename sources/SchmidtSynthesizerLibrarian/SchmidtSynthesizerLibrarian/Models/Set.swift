//
//  Set.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class Set: NamedObject {

    var data:NSData?
    var banks:[Bank]=[]
    
    override init(name:String){
        super.init(name:name)
    }

}
