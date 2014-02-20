
public class Application {

	public static void main(String[] args) {
		Table table = new Table("/home/thib/Projects/Workspaces/Eclipse/Subgroup/cereal.csv",
								new String[] {"calories","protein","fat","sodium","fiber","carbo",
								 "sugars","potass","vitamins","shelf","weight","cups","rating"},
								 "rating");
	
		Explorer explorer = new Explorer(table);
		explorer.explore();
	}

}