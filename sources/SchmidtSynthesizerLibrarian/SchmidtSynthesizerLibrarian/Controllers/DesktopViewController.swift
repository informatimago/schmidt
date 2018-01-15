//
//  DesktopViewController.swift
//  SchmidtSynthesizerLibrarian
//
//  Created by Pascal Bourguignon on 15/01/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

import UIKit

class DesktopViewController: UIViewController {

    @IBOutlet weak var desktopView: DesktopView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        desktopView.elements.append(DesktopInstance(frame:CGRect(x:10,y:10,width:100,height:30),
                                                    name:"PJB Test",
                                                    object:Program(name:"PJB Test")))
        
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
    }


}
