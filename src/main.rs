use std::cmp;
use std::fmt;
use std::io;
use std::ops::{Index, IndexMut};
use std::process::exit;
use std::time::{Instant, Duration};
use rand::{thread_rng, Rng};
use self::Visibility::*;
use self::GameError::*;
use self::GameStatus::*;

/* basic objects */

#[derive(Debug, Clone, Copy, PartialEq)]
struct Coords { x: usize, y: usize }

impl Coords {
    fn new(x: usize, y: usize) -> Coords {
        Coords { x, y }
    }

    fn new_io(c1: &str, c2: &str, max: usize) -> Result<Coords, GameError> {
        let (x0, y0) = (c1.parse(), c2.parse());
        if x0.is_err() || y0.is_err() { return Err(InvalidCoords) };
        let (x, y) = (x0.unwrap(), y0.unwrap());

        if x > max || y > max || x == 0 || y == 0 {
            Err(InvalidCoords)
        } else {
            Ok(Coords::new(x, y))
        }
    }

    fn neighbors(&self, max: usize) -> Vec<Coords> {
        let mut neighbors = Vec::with_capacity(8);

        let (min_x, min_y) = (cmp::max(self.x - 1, 1), cmp::max(self.y - 1, 1));
        let (max_x, max_y) = (cmp::min(self.x + 1, max), cmp::min(self.y + 1, max));

        for x in min_x..=max_x {
            for y in min_y..=max_y {
                if !(x == self.x && y == self.y) {
                    neighbors.push(Coords::new(x, y));
                }
            }
        }

        neighbors
    }
}

impl From<(usize, usize)> for Coords {
    fn from((x, y): (usize, usize)) -> Self {
        Coords::new(x, y)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Visibility {
    Hidden,
    Visible,
    Flagged
}

#[derive(Debug, PartialEq)]
struct Field {
    coords: Coords,
    visibility: Visibility,
    rigged: bool,
    number: usize,
    exploded: bool
}

impl Field {
    fn new<T: Into<Coords>>(coords: T) -> Field {
        Field {
            coords: coords.into(),
            visibility: Hidden,
            rigged: false,
            number: 0,
            exploded: false
        }
    }

    fn flag(&mut self) -> Result<(), GameError> {
        match self.visibility {
            Visible => Err(FieldAlreadyVisible),
            Hidden => { self.visibility = Flagged; Ok(()) }, // can only flag Hidden fields
            Flagged => { self.visibility = Hidden; Ok(()) }
        }
    }
}

impl<T: Into<Coords> + Copy> Index<T> for Grid {
    type Output = Field;

    fn index(&self, coords: T) -> &Field {
        self.fields.iter().find(|f| f.coords == coords.into())
            .expect("cannot borrow given field or it doesn't exist")
    }
}

impl<T: Into<Coords> + Copy> IndexMut<T> for Grid {
    fn index_mut(&mut self, coords: T) -> &mut Field {
        self.fields.iter_mut().find(|f| f.coords == coords.into())
            .expect("cannot mutably borrow given field or it doesn't exist")
    }
}

#[derive(Debug)]
struct Grid {
    size: usize,
    fields: Vec<Field>
}

impl Grid {
    fn new(size: usize) -> Grid {
        let mut fields = Vec::with_capacity(size);

        for x in 1..=size {
            for y in 1..=size {
                fields.push(Field::new((x, y)));
            }
        }

        Grid {
            size,
            fields
        }
    }

    fn rig(&mut self, density: f64) {
        let mut rng = thread_rng();

        for x in 1..=self.size {
            for y in 1..=self.size {
                self[(x, y)].rigged = rng.gen_bool(density);
            }
        }

        for x in 1..=self.size {
            for y in 1..=self.size {
                self[(x, y)].number = self.mines_around((x, y));
            }
        }
    }

    fn mines_around<T: Into<Coords>>(&self, coords: T) -> usize {
        let neighboring_coords = coords.into().neighbors(self.size);
        self.fields.iter().filter(|f| neighboring_coords.contains(&f.coords) && f.rigged).count()
    }

    fn uncover<T: Into<Coords> + Copy>(&mut self, coords: T) -> Result<(), GameError> {
        match self[coords].visibility {
            Visible => Err(FieldAlreadyVisible),
            Flagged => Err(FieldIsFlagged),
            Hidden  => {
                self[coords].visibility = Visible;
                if self[coords].rigged { self[coords].exploded = true; }
                // clear empty neighbors
                if !self[coords].rigged && self[coords].number == 0 {
                    for c in coords.into().neighbors(self.size) {
                        if !self[c].rigged && self[c].visibility == Hidden {
                            let _ = self.uncover(c);
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

struct Time(Duration);

/* display */

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.visibility == Flagged {
            write!(f, "█")
        } else if self.visibility == Hidden  {
            write!(f, "░")
        } else if self.exploded {
            write!(f, "X")
        } else if self.rigged {
            write!(f, "•")
        } else if self.number == 0 {
            write!(f, " ")
        } else {
            write!(f, "{}", self.number)
        }
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let limit = self.size + 1;
        let (border_hor, border_ver, border_cross) = ('═', '║', '╬');

        // upper/lower numbers
        let mut numbers = String::new();
        numbers.push_str("  ");
        for n in 1..limit {
            numbers.push_str(&format!("{}{:^3}", border_ver, n));
        }
        numbers.push(border_ver);

        // row line
        let mut row = String::new();
        row.push_str(&format!("{0}{0}{1}", border_hor, border_cross));
        for _ in 1..limit {
            row.push_str(&format!("{0}{0}{0}{1}", border_hor, border_cross));
        }
        row.push_str(&format!("{0}{0}", border_hor));

        // grid
        writeln!(f, "{}", numbers)?;
        for x in 1..limit {
            writeln!(f, "{}", row)?;
            write!(f, "{:<2}", x)?;
            for y in 1..limit {
                write!(f, "{} {} ", border_ver, self[(x, y)])?;
            }
            writeln!(f, "{}{:2}", border_ver, x)?;
        }
        writeln!(f, "{}", row)?;
        writeln!(f, "{}", numbers)
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}m{}s", self.0.as_secs() / 60, self.0.as_secs() % 60)
    }
}

impl fmt::Display for GameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InvalidCoords => write!(f, "invalid coordinates!"),
            InvalidInput => write!(f, "invalid command!"),
            FieldAlreadyVisible => write!(f, "field already visible!"),
            FieldIsFlagged => write!(f, "field is flagged!")
        }
    }
}

impl fmt::Display for GameStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            GameOver => write!(f, "game over"),
            Victory => write!(f, "victory"),
            _ => unreachable!()
        }
    }
}

/* the game */

struct Game {
    grid: Grid,
    buffer: String,
    time: Instant,
    last: Option<Coords>
}

#[derive(Debug, PartialEq)]
enum GameStatus {
    Proceed,
    GameOver,
    Victory
}

#[derive(Debug, PartialEq)]
enum GameError {
    InvalidInput,
    InvalidCoords,
    FieldAlreadyVisible,
    FieldIsFlagged
}

impl Game {
    fn new(size: usize, difficulty: f64) -> Game {
        let mut grid = Grid::new(size);
        grid.rig(difficulty);

        Game {
            grid,
            buffer: String::with_capacity(32),
            time: Instant::now(),
            last: None
        }
    }

    fn turn(&mut self) {
        let status = self.check_status();
        if status == GameOver {
            for f in self.grid.fields.iter_mut() { f.visibility = Visible }
        }

        println!("\n{}", self.grid);

        match status {
            GameOver | Victory => {
                println!("{}! time elapsed: {}", status, Time(self.time.elapsed()));
                let _ = io::stdin().read_line(&mut self.buffer).unwrap();
                exit(0)
            },
            Proceed => ()
        }

        if let Err(e) = self.handle_input() { println!("{}", e) }
    }

    fn check_status(&self) -> GameStatus {
        if self.last.is_some() && self.grid[self.last.unwrap()].exploded {
            GameOver
        } else if self.grid.fields.iter().filter(|f| f.visibility == Visible).count()
               == self.grid.fields.iter().filter(|f| !f.rigged).count()
        {
            Victory
        } else {
            Proceed
        }
    }

    fn handle_input(&mut self) -> Result<(), GameError> {
        self.buffer.clear();
        let _ = io::stdin().read_line(&mut self.buffer).unwrap();

        let tmp_buffer = self.buffer.clone();
        let words = tmp_buffer.split_whitespace().collect::<Vec<&str>>();

        if words.is_empty() {
            Err(InvalidInput)
        } else if ["resign", "exit", "quit"].contains(&words[0]) {
            exit(0)
        } else {
            if words.len() != 3 { return Err(InvalidInput) };

            let coords = Coords::new_io(&words[1], &words[2], self.grid.size)?;
            self.last = Some(coords); // if the command is "flag", this just won't do anything

            match words[0] {
                "uncover" | "u" => self.grid.uncover(coords),
                "flag" | "f"    => self.grid[coords].flag(),
                _               => Err(InvalidInput)
            }
        }
    }
}

fn main() {
    let mut game = Game::new(10, 0.25);
    loop { game.turn() }
}

/* tests */

#[cfg(test)]
mod test {
    use super::Coords;
    use super::Field;
    use super::Grid;
    use super::GameError::*;
    use super::Visibility::*;

    fn staged_grid(size: usize, mines: &[Coords]) -> Grid {
        let mut grid = Grid::new(size);

        for &coords in mines { grid[coords].rigged = true }

        for x in 1..(size + 1) {
            for y in 1..(size + 1) {
                grid[(x, y)].number = grid.mines_around((x, y));
            }
        }

        grid
    }

    #[test]
    fn creating_coords() {
        assert_eq!(Ok(Coords::new( 1,  1)), Coords::new_io( "1",  "1", 10));
        assert_eq!(Ok(Coords::new(10, 10)), Coords::new_io("10", "10", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io("11", "10", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io("10", "11", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io( "1",   "", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io(  "",  "1", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io(  "",   "", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io("1a",  "1", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io("a1",  "1", 10));
        assert_eq!(Err(InvalidCoords), Coords::new_io( "a",  "1", 10));
    }

    #[test]
    fn coord_neighbors() {
        assert_eq!(Coords::new(1, 1).neighbors(2).len(), 3);
        assert_eq!(Coords::new(1, 2).neighbors(2).len(), 3);
        assert_eq!(Coords::new(2, 1).neighbors(2).len(), 3);
        assert_eq!(Coords::new(2, 2).neighbors(2).len(), 3);
        assert_eq!(Coords::new(1, 2).neighbors(3).len(), 5);
        assert_eq!(Coords::new(2, 1).neighbors(3).len(), 5);
        assert_eq!(Coords::new(2, 2).neighbors(3).len(), 8);
        assert_eq!(Coords::new(2, 3).neighbors(3).len(), 5);
        assert_eq!(Coords::new(3, 2).neighbors(3).len(), 5);
    }

    #[test]
    fn indexing_grid() {
        let grid = staged_grid(10, &[Coords::new(1, 1), Coords::new(5, 5), Coords::new(10, 10)]);

        assert!(grid[( 1,  1)].rigged);
        assert!(grid[( 5,  5)].rigged);
        assert!(grid[(10, 10)].rigged);
    }

    #[test]
    fn flagging() {
        let mut f = Field::new((1, 1));
        assert_eq!(f.visibility, Hidden);
        let _ = f.flag();
        assert_eq!(f.visibility, Flagged);
        let _ = f.flag();
        assert_eq!(f.visibility, Hidden);
    }

    #[test]
    fn counting_mines() {
        let grid = staged_grid(3, &[Coords::new(1, 1), Coords::new(3, 3)]);

        assert_eq!(grid[(1, 1)].number, 0);
        assert_eq!(grid[(1, 2)].number, 1);
        assert_eq!(grid[(1, 3)].number, 0);
        assert_eq!(grid[(2, 1)].number, 1);
        assert_eq!(grid[(2, 2)].number, 2);
        assert_eq!(grid[(2, 3)].number, 1);
        assert_eq!(grid[(3, 1)].number, 0);
        assert_eq!(grid[(3, 2)].number, 1);
        assert_eq!(grid[(3, 3)].number, 0);
    }

    #[test]
    fn uncovering_tight_space() {
        let mut grid = staged_grid(3, &[Coords::new(2, 1)]);
        let _ = grid.uncover((1, 1));

        assert_eq!(grid[(1, 1)].visibility, Visible);
        assert_eq!(grid[(1, 2)].visibility, Hidden);
        assert_eq!(grid[(1, 3)].visibility, Hidden);
        assert_eq!(grid[(2, 1)].visibility, Hidden);
        assert_eq!(grid[(2, 2)].visibility, Hidden);
        assert_eq!(grid[(2, 3)].visibility, Hidden);
        assert_eq!(grid[(3, 1)].visibility, Hidden);
        assert_eq!(grid[(3, 2)].visibility, Hidden);
        assert_eq!(grid[(3, 3)].visibility, Hidden);
    }

    #[test]
    fn uncovering_open_space() {
        let mut grid = staged_grid(3, &[Coords::new(3, 3)]);
        let _ = grid.uncover((1, 1));

        assert_eq!(grid[(1, 1)].visibility, Visible);
        assert_eq!(grid[(1, 2)].visibility, Visible);
        assert_eq!(grid[(1, 3)].visibility, Visible);
        assert_eq!(grid[(2, 1)].visibility, Visible);
        assert_eq!(grid[(2, 2)].visibility, Visible);
        assert_eq!(grid[(2, 3)].visibility, Visible);
        assert_eq!(grid[(3, 1)].visibility, Visible);
        assert_eq!(grid[(3, 2)].visibility, Visible);
        assert_eq!(grid[(3, 3)].visibility, Hidden);
    }
}
