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


    func typeLetter(_ instance:NamedObject) -> String {
        if(instance is Program)  {return "P"}
        if(instance is Bank)     {return "B"}
        if(instance is BankSet)  {return "S"}
        return "X"
    }

    var pos=10

    func add(instance:NamedObject){
        desktopView.add(element:DesktopInstance(frame:CGRect(x:10,y:pos,width:200,height:40),
                                                name:typeLetter(instance)+": "+instance.name,
                                                object:instance))
        pos+=40
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        add(instance:Program(name:"PJB Test"))
        add(instance:Program(name:"ABCDEFGHIJKLMNOPQR"))
        add(instance:Bank(name:"Pads"))
        add(instance:BankSet(name:"Concert in Paris"))
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
    }


}


