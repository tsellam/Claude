public class Evaluator {
	

	// Default score: WRacc
	public float score(Rule rule, Table table){
		
		float[] subgroup = rule.applyTo(table);
		float[] target	 = table.getTarget();
		
		float nSubgroup 	= (float) subgroup.length, 
		      nTarget 		= (float) target.length,
		      nPosSubgroup 	= 0,
		      nPosTarget 	= 0;
		
		for (float i : subgroup) nPosSubgroup += i;
		for (float i : target) 	 nPosTarget += i;
		
		float fSubgroup    = nSubgroup / nTarget,
			  fPosSubgroup = nPosSubgroup / nSubgroup,
			  fPosTarget   = nPosTarget / nTarget,
			  wrAcc 	   = fSubgroup * (fPosSubgroup - fPosTarget);
		
		System.out.println("");
		System.out.println("Evaluating " + rule.toString());
		System.out.println(nSubgroup + " / "+ nTarget +", nPosSubgroup: " + nPosSubgroup + ", nPosTarget: " + nPosTarget);
		System.out.println(wrAcc);
		
		return wrAcc;
	}

}
