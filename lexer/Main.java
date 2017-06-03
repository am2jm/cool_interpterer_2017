import java.io.*;

public class Main{
    /* This is really bad code; strive for better! */
    public static void main(String[] args) throws IOException {
    	String filename = args[0];
//		File f = new File(filename + "-lex");
    String tokens = "";
    boolean myFlag = false;

		File file = new File(filename + "-lex");
 		if (file.exists()){
     		file.delete();
 		}

		FileWriter fw = new FileWriter(filename+"-lex", true);
		BufferedWriter bw = new BufferedWriter(fw);

		String line = null;
		FileReader fileReader = new FileReader(filename);
		// Always wrap FileReader in BufferedReader.
		BufferedReader bufferedReader = new BufferedReader(fileReader);
		String in = "";
    boolean makeFile = true;
		while((line = bufferedReader.readLine()) != null) {
			in += line + "\n";
		}

    	Lexer l = new Lexer(new StringReader(in));

		try {
			while(true) {
				Token t = l.yylex();
				if(t == null) // we're done
					break;
				else
				{
					//System.out.println(t);
					// bw.write(t.toString());
					// bw.write("\n");
          if(t.hadError()){
            myFlag = true;
            System.out.println("ERROR: " + t.getLine() + ": Lexer: " + t.getMessage());
            break;
          }
          else{
          tokens += t.toString();
          tokens += "\n";
          }
				}
			}
		} catch (Error e){
			System.out.println(e.getMessage());
		}
    if(!myFlag){
      bw.write(tokens);
    }
		bw.close();
    	bufferedReader.close();
          // Always close files.


//        BufferedReader br = new BufferedReader(
//                              new InputStreamReader(System.in));
//
//        while(true) {
//            System.out.println("Enter equation (stop with newline)");
//
//
//        }
    }
}
