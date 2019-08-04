//
//  Bank.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class Bank: NamedObject {

    var data:NSData?
    var programs:[Program]=[]

    required init(name:String){
        super.init(name:name)
    }

}
