import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

public class Table {

	// Java stuff
	private float[][] data;
	private String[] columns;
	private String   target;
	
	
	Table(float[][] input) {
		this.data = input;
	}
	
	Table (String fileName, String[] columns, String target){
	    this.columns = columns;
	    this.target  = target;
		this.readFile(fileName);
	    this.discretizeTarget();
	}
	
	public String toString(){
		String out = "", tuple = "";
		for (int j = 0; j < data[0].length; j++){
			tuple = "";
			for (int i = 0; i < data.length; i++){
				tuple += Float.toString(data[i][j]);
				if (i < data.length -1) tuple += "\t";
			}
			out += tuple + "\n";
		}
		return out;
	}
	
	/***************/
	/* Data access */
	/***************/
	// Column name/index conversion
	public int getCid(String colName){
		for (int i = 0; i < columns.length; i++)
			if (columns[i].equals(colName)) return i;
		return -1;
	}
	
	public String getCname(int i){
		return columns[i];
	}
	
	// Get data column IDs/ target column IDs
	public int[] getDataIds() {
		int[] indices  = new int[this.columns.length - 1];
		for (int i=0; i < this.columns.length; i++)
			if (i != getCid(this.target)) indices[i] = i;
		return indices;
	}
	
	public int getTargetId(){
		return this.getCid(this.target);
	}
	
	// Access the actual data values
	public float[] getCol(int j){
		return this.data[j];
	}
	
	public float[] getCol(String name){
		return this.data[getCid(name)];
	}
	
	public float[] getTarget(){
		return getCol(this.target);
	}

	// Count
	public int getnCols(){
		return this.data.length;
	}
	
	public int getnRows(){
		return this.data[0].length;
	}


	
	/**********/
	/* Utils  */
	/**********/
	public void discretizeTarget(){
		
		float[] initVals   = this.getTarget();
		float[] sortedVals = initVals.clone();
		
		Arrays.sort(sortedVals);
		float threshold = sortedVals[(int) (sortedVals.length * 0.75)];
		
		for (int i = 0; i < data[0].length; i++)
			initVals[i] = initVals[i] >= threshold ? 1 : 0;
	}
		
	public void readFile(String fileName){
		
	    System.out.println("Reading file");
		try(BufferedReader br = new BufferedReader(new FileReader(fileName))) {

	    	ArrayList<float[]> buffer = new ArrayList<float[]>();
	        String[] rawTuple = null;
	    	float[]  tuple    = null;
	    	
	    	// For each line
	    	String line = br.readLine();
	        while (line != null) {
	        	// Parses
	            rawTuple 	= line.split(",");
	            tuple 		= new float[rawTuple.length];
	            for (int i = 0; i < rawTuple.length; i++) 
	            	tuple[i] = (float) Float.parseFloat(rawTuple[i]);
	            buffer.add(tuple);
	            
	            // Reads next
	            line = br.readLine();
	        }
	        
	        // Copies in a float[][] array
	        data = new float[tuple.length][buffer.size()];
    		for (int i = 0; i < tuple.length; i++)
    			for (int j = 0; j < buffer.size(); j++)
    				data[i][j] = buffer.get(j)[i];
	        
	    } catch (IOException e) {
			e.printStackTrace();
		}
	}
	
}