import java.util.Arrays;
import java.util.TreeSet;

public class Explorer {

	// Constructors and stuff
	Table table;
	int nBins;
	int nRulesMax;
	int beamWidth;
	Evaluator eval;

	Explorer(Table t){
		this.table  	= t;
		this.nBins  	= 2;
		this.nRulesMax  = 3;
		this.beamWidth  = 50;
		this.eval 		= new Evaluator();
	}
	
	Bin[] getBins(String colName, float[] col){
		
		// Initialization
		Bin[] bins = new Bin[nBins];
		
		// Sorts the array
		float[] sorted = col.clone();
		Arrays.sort(sorted);
		
		// Creates the bins
		int b = 0, binW = (int) Math.ceil((double) col.length / nBins);
		for (int i = 0; i < col.length; i += binW){
			int upperB = (int) Math.min(i + binW - 1, col.length - 1);
			bins[b++] = new Bin(colName, sorted[i], sorted[upperB]);
		}
		
		return bins;
	}
	
	// Actual SD function
	public void explore(){
		
		// Initializes
		int level = 0;
		Beam initBeam = new Beam(this.beamWidth), beam = null;
		initBeam.add(new Rule());
		
		// For each level
		while (level++ < nRulesMax){
			
			beam = new Beam(this.beamWidth);
			
			// For each rule in the beam
			for (Rule radix : initBeam){
			
				// For each (non-target) column
				int[] dataIndices = this.table.getDataIds();
				for (int col : dataIndices){
					
					// Passes if col is already in the rule
					if (radix.containsColumn(table.getCname(col))) continue;
				
					// For each bin
					Bin[] bins  = getBins(table.getCname(col), table.getCol(col));
					for (Bin bin : bins){
						
						// Creates, scores and records the new rule
						Rule candidate = radix.appendPredicate(bin);
						candidate.setScore(table, eval);
						beam.add(candidate);
					}
				}
			}
			initBeam = beam;
		}
		for (Rule r : beam) System.out.println(r);
	}
	
		
	class Beam extends TreeSet<Rule> {
		
		private static final long serialVersionUID = 1L;
		private int width;
		
		Beam(int width) {
			super();
			this.width = width;
		}
		
		public boolean add(Rule r){
			if (super.size() >= this.width){
				Rule min = super.pollFirst();
				if (r.compareTo(min) > 0) {
					return super.add(r);
				} else { 
					super.add(min);
					return false;
				}	
			} else {
				return super.add(r);
			}
		}
		
	}

}
