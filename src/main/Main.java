package main;

import jess.JessException;
import jess.Rete;

public class Main {
	
	public static Rete engine = null;
	
	public Main() {
		
		try {
			engine = new Rete();
			engine.batch("main/main.clp");
		} catch (JessException e) {
			e.printStackTrace();
		}
		
	}
	
	public static void main(String[] args) {
		new Main();
	}
}
