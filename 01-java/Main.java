import java.io.*;
import java.util.ArrayList;

import static java.lang.Math.abs;
import static java.lang.Math.min;
import static java.lang.Math.max;

public class Main {
    public ArrayList<Move> moves = new ArrayList<>();
    public static void main(String[] args) throws FileNotFoundException, IOException {
        new Main();
    }

    public Main() throws FileNotFoundException, IOException {
        var reader = new BufferedReader(new FileReader("./input.txt"));
        String str = "";
        while ((str = reader.readLine()) != null) {
            for (String s : str.split(","))
                parse(s.trim());
        }
        reader.close();

        System.out.println("Part 1: " + solve(false));
        System.out.println("Part 2: " + solve(true));
    }

    private void parse(String str) {
        char dir = str.charAt(0);
        String length = str.substring(1);
        int len = Integer.parseInt(length);
        moves.add(new Move(dir, len));
    }

    private record Point(int x, int y) {}

    private int solve(boolean part2) {
        int x = 0;
        int y = 0;
        int dir = 0;

        var visited = new ArrayList<Point>();

        for (Move m : moves) {
            switch (m.dir()) {
            case 'R':
                dir = (dir + 1) % 4;
                break;
            case 'L':
                dir = (dir + 4 - 1) % 4;
                break;
            }
            switch(dir) {
            case 0:
                for (int i = 0; i < m.length(); i++) {
                    var p = new Point(x, y--);
                    if (visited.contains(p) && part2) return abs(p.x)+abs(p.y);
                    visited.add(p);
                }
                break;
            case 1:
                for (int i = 0; i < m.length(); i++) {
                    var p = new Point(x++, y);
                    if (visited.contains(p) && part2) return abs(p.x)+abs(p.y);
                    visited.add(p);
                }
                break;
            case 2:
                  for (int i = 0; i < m.length(); i++) {
                    var p = new Point(x, y++);
                    if (visited.contains(p) && part2) return abs(p.x)+abs(p.y);
                    visited.add(p);
                }
                break;
            case 3:
                for (int i = 0; i < m.length(); i++) {
                var p = new Point(x--, y);
                if (visited.contains(p) && part2) return abs(p.x)+abs(p.y);
                    visited.add(p);
                }
                break;
            }
        }

        if (part2) System.out.println(visited);

        return abs(x)+abs(y);
    }

    private record Move(char dir, int length) {}
}
