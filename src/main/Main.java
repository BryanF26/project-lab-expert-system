package main;

import jess.Rete;

public class Main {
	
	public static Rete engine = null;
	
	public Main() {
		
		try {
			engine = new Rete();
			engine.batch("");
		} catch (Exception e) {
			// TODO: handle exception
		}
		
	}
	
	public static void main(String[] args) {
		new Main();
	}
}
