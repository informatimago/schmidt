//
//  Program.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class Program: NamedObject {

    var data:NSData?
    var category:Category?

    required init(name:String){
        super.init(name:name)
    }

}
