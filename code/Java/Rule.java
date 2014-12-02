import java.util.ArrayList;


public class Rule implements Comparable<Rule> {
	
	ArrayList<Bin> predicates;
	float score;
	
	Rule(){
		this.predicates = new ArrayList<Bin>();
		this.score 		= -1; 
	}
	
	Rule (Rule r){
		this.predicates = new ArrayList<Bin>(r.getPredicates());
		this.score 		= -1; 
	}
	
	ArrayList<Bin> getPredicates(){
		return this.predicates;
	}
	
	float getScore(){
		return this.score;
	}
	
	public String toString(){
		String out = "";
		for (Bin b : predicates) out += b.toString() + " \t ";
		out += "| Score : " + this.score;
		return out;
	}
	
	public int compareTo(Rule r) {
		return Float.compare(this.score, r.getScore());
	}
	
	
	
	void appendBin(Bin b){
		this.predicates.add(b);
	}
	
	public boolean containsColumn(String col){
		boolean t = false;
		for (Bin b : this.predicates) t = t || (b.getColName() == col);
		return t;
	}
	
	Rule appendPredicate(Bin bin){
		Rule newRule = new Rule(this);
		newRule.appendBin(bin);
		return newRule;
	}
	
	
	public float[] applyTo(Table table){
		// Initializes an array of Booleans
		boolean[] select = new boolean[table.getnRows()];
		for (int i = 0; i < table.getnRows(); i++) select[i] = true;
		
		// ANDs it with each predicate
		for (Bin bin : this.predicates){
			boolean[] selectBin = bin.applyTo(table);
			for (int i = 0; i < table.getnRows(); i++)
				select[i] = select[i] & selectBin[i];
		}
		// Gets the count
		int count = 0;
		for (int i = 0; i < table.getnRows(); i++) 
			if(select[i]) count++;
		
		// Writes the array
		float[] out = new float[count];
		int	 	j   = 0;
		for (int i = 0; i < table.getnRows(); i++) 
			if(select[i]) out[j++] = table.getTarget()[i];
		
		return out;
	}
	
	public void setScore(Table table, Evaluator eval){
		this.score = eval.score(this, table);
	}

}
