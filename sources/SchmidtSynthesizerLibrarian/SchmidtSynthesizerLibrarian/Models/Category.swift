//
//  Category.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 14/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class Category: NSObject {
    var name:String
    var color:Int
    init(color:Int,name:String){
        self.name=name
        self.color=color
    }
}
