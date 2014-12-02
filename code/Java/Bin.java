public class Bin {
	
	String colName;
	float min, max;
	
	Bin(String colName, float min, float max){
		this.colName = colName;
		this.min = min;
		this.max = max;
	}
	
	public String getColName(){
		return this.colName;
	}
	
	public String toString(){
		return this.colName +": " + this.min + ", " + this.max;
	}
	
	public boolean[] applyTo(Table table){
		float[] column   = table.getCol(this.colName);
		boolean[] select = new boolean[column.length];
		for (int i=0; i < column.length; i++)
			select[i] = (column[i] >= this.min) && (column[i] <= this.max);
		return select;
	}
}
