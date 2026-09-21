//! A text buffer with a cursor, shared by the query line and the editor.

/// A cursor movement.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Motion {
    /// One character left.
    Left,
    /// One character right.
    Right,
    /// To the start of the previous word.
    WordLeft,
    /// To the start of the next word.
    WordRight,
    /// One line up.
    Up,
    /// One line down.
    Down,
    /// Start of the line.
    Home,
    /// End of the line.
    End,
}

/// One line-editing action.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Edit {
    /// Type a character.
    Insert(char),
    /// Insert a line break.
    Newline,
    /// Delete the character before the cursor.
    DeleteBackward,
    /// Delete the character under the cursor.
    DeleteForward,
    /// Delete the word before the cursor, keeping it for yank.
    DeleteWordBackward,
    /// Delete the word after the cursor, keeping it for yank.
    DeleteWordForward,
    /// Delete from the start of the line to the cursor, keeping it for yank.
    DeleteToStart,
    /// Re-insert the last deleted text.
    Yank,
    /// Move the cursor.
    Move(Motion),
}

/// Lines of text with a cursor and a one-slot kill ring.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TextBuffer {
    lines: Vec<String>,
    row: usize,
    col: usize,
    killed: String,
}

impl TextBuffer {
    /// Opens on `text`, cursor at the end.
    pub fn new(text: &str) -> Self {
        let lines: Vec<String> = if text.is_empty() {
            vec![String::new()]
        } else {
            text.lines().map(ToOwned::to_owned).collect()
        };
        let row = lines.len() - 1;
        let col = lines[row].chars().count();
        Self {
            lines,
            row,
            col,
            killed: String::new(),
        }
    }

    /// The buffer's contents.
    pub fn text(&self) -> String {
        self.lines.join("\n")
    }

    /// The buffer's lines.
    pub fn lines(&self) -> &[String] {
        &self.lines
    }

    /// The cursor, as `(row, column)` in characters.
    pub const fn cursor(&self) -> (usize, usize) {
        (self.row, self.col)
    }

    /// Whether the buffer holds nothing.
    pub fn is_empty(&self) -> bool {
        self.lines.iter().all(String::is_empty)
    }

    /// Applies one edit.
    pub fn apply(&mut self, edit: &Edit) {
        match edit {
            Edit::Insert(c) => self.insert(*c),
            Edit::Newline => self.newline(),
            Edit::DeleteBackward => self.delete_backward(),
            Edit::DeleteForward => self.delete_forward(),
            Edit::DeleteWordBackward => self.delete_word_backward(),
            Edit::DeleteWordForward => self.delete_word_forward(),
            Edit::DeleteToStart => self.delete_to_start(),
            Edit::Yank => self.yank(),
            Edit::Move(motion) => self.move_cursor(*motion),
        }
    }

    fn width(&self, row: usize) -> usize {
        self.lines[row].chars().count()
    }

    fn byte_at(&self, row: usize, col: usize) -> usize {
        self.lines[row]
            .char_indices()
            .nth(col)
            .map_or(self.lines[row].len(), |(i, _)| i)
    }

    fn insert(&mut self, c: char) {
        let at = self.byte_at(self.row, self.col);
        self.lines[self.row].insert(at, c);
        self.col += 1;
    }

    fn newline(&mut self) {
        let at = self.byte_at(self.row, self.col);
        let tail = self.lines[self.row].split_off(at);
        self.lines.insert(self.row + 1, tail);
        self.row += 1;
        self.col = 0;
    }

    fn delete_backward(&mut self) {
        if self.col > 0 {
            let at = self.byte_at(self.row, self.col - 1);
            self.lines[self.row].remove(at);
            self.col -= 1;
        } else if self.row > 0 {
            let line = self.lines.remove(self.row);
            self.row -= 1;
            self.col = self.width(self.row);
            self.lines[self.row].push_str(&line);
        }
    }

    fn delete_forward(&mut self) {
        if self.col < self.width(self.row) {
            let at = self.byte_at(self.row, self.col);
            self.lines[self.row].remove(at);
        } else if self.row + 1 < self.lines.len() {
            let next = self.lines.remove(self.row + 1);
            self.lines[self.row].push_str(&next);
        }
    }

    /// The column where the word before the cursor starts.
    fn word_start(&self, row: usize, col: usize) -> usize {
        let chars: Vec<char> = self.lines[row].chars().collect();
        let mut at = col;
        while at > 0 && chars[at - 1].is_whitespace() {
            at -= 1;
        }
        while at > 0 && !chars[at - 1].is_whitespace() {
            at -= 1;
        }
        at
    }

    /// The column where the word after the cursor ends.
    fn word_end(&self, row: usize, col: usize) -> usize {
        let chars: Vec<char> = self.lines[row].chars().collect();
        let mut at = col;
        while at < chars.len() && chars[at].is_whitespace() {
            at += 1;
        }
        while at < chars.len() && !chars[at].is_whitespace() {
            at += 1;
        }
        at
    }

    /// Removes `from..to` on the current line and keeps it for yank.
    fn cut(&mut self, from: usize, to: usize) {
        let start = self.byte_at(self.row, from);
        let end = self.byte_at(self.row, to);
        self.killed = self.lines[self.row][start..end].to_owned();
        self.lines[self.row].replace_range(start..end, "");
        self.col = from;
    }

    fn delete_word_backward(&mut self) {
        if self.col == 0 {
            self.delete_backward();
            return;
        }
        let start = self.word_start(self.row, self.col);
        self.cut(start, self.col);
    }

    fn delete_word_forward(&mut self) {
        let end = self.word_end(self.row, self.col);
        if end > self.col {
            let col = self.col;
            self.cut(col, end);
        }
    }

    fn delete_to_start(&mut self) {
        if self.col > 0 {
            let col = self.col;
            self.cut(0, col);
        }
    }

    fn yank(&mut self) {
        for c in self.killed.clone().chars() {
            self.insert(c);
        }
    }

    fn move_cursor(&mut self, motion: Motion) {
        match motion {
            Motion::Left => {
                if self.col > 0 {
                    self.col -= 1;
                } else if self.row > 0 {
                    self.row -= 1;
                    self.col = self.width(self.row);
                }
            }
            Motion::Right => {
                if self.col < self.width(self.row) {
                    self.col += 1;
                } else if self.row + 1 < self.lines.len() {
                    self.row += 1;
                    self.col = 0;
                }
            }
            Motion::WordLeft => self.col = self.word_start(self.row, self.col),
            Motion::WordRight => self.col = self.word_end(self.row, self.col),
            Motion::Up => {
                if self.row > 0 {
                    self.row -= 1;
                    self.col = self.col.min(self.width(self.row));
                }
            }
            Motion::Down => {
                if self.row + 1 < self.lines.len() {
                    self.row += 1;
                    self.col = self.col.min(self.width(self.row));
                }
            }
            Motion::Home => self.col = 0,
            Motion::End => self.col = self.width(self.row),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Edit, Motion, TextBuffer};

    fn buffer(text: &str) -> TextBuffer {
        TextBuffer::new(text)
    }

    fn apply(buf: &mut TextBuffer, edits: &[Edit]) {
        for edit in edits {
            buf.apply(edit);
        }
    }

    #[test]
    fn opens_with_the_cursor_at_the_end() {
        let buf = buffer("echo hello");
        assert_eq!(buf.text(), "echo hello");
        assert_eq!(buf.cursor(), (0, 10));
    }

    #[test]
    fn home_and_end_jump_to_the_line_edges() {
        let mut buf = buffer("echo hello");
        buf.apply(&Edit::Move(Motion::Home));
        assert_eq!(buf.cursor(), (0, 0));
        buf.apply(&Edit::Move(Motion::End));
        assert_eq!(buf.cursor(), (0, 10));
    }

    #[test]
    fn word_motions_step_over_words() {
        let mut buf = buffer("one two three");
        buf.apply(&Edit::Move(Motion::WordLeft));
        assert_eq!(buf.cursor(), (0, 8));
        buf.apply(&Edit::Move(Motion::WordLeft));
        assert_eq!(buf.cursor(), (0, 4));
        buf.apply(&Edit::Move(Motion::WordRight));
        assert_eq!(buf.cursor(), (0, 7));
    }

    #[test]
    fn delete_forward_removes_under_the_cursor() {
        let mut buf = buffer("abc");
        buf.apply(&Edit::Move(Motion::Home));
        buf.apply(&Edit::DeleteForward);
        assert_eq!(buf.text(), "bc");
        assert_eq!(buf.cursor(), (0, 0));
    }

    #[test]
    fn delete_forward_at_the_end_does_nothing() {
        let mut buf = buffer("abc");
        buf.apply(&Edit::DeleteForward);
        assert_eq!(buf.text(), "abc");
    }

    #[test]
    fn delete_word_backward_takes_the_previous_word() {
        let mut buf = buffer("one two three");
        buf.apply(&Edit::DeleteWordBackward);
        assert_eq!(buf.text(), "one two ");
        assert_eq!(buf.cursor(), (0, 8));
    }

    #[test]
    fn delete_word_backward_skips_trailing_spaces() {
        let mut buf = buffer("one two   ");
        buf.apply(&Edit::DeleteWordBackward);
        assert_eq!(buf.text(), "one ");
    }

    #[test]
    fn delete_word_forward_takes_the_next_word() {
        let mut buf = buffer("one two three");
        buf.apply(&Edit::Move(Motion::Home));
        buf.apply(&Edit::DeleteWordForward);
        assert_eq!(buf.text(), " two three");
    }

    #[test]
    fn delete_to_start_clears_the_line_before_the_cursor() {
        let mut buf = buffer("one two");
        buf.apply(&Edit::DeleteToStart);
        assert_eq!(buf.text(), "");
        assert_eq!(buf.cursor(), (0, 0));
    }

    #[test]
    fn yank_restores_the_last_deletion() {
        let mut buf = buffer("one two");
        buf.apply(&Edit::DeleteWordBackward);
        assert_eq!(buf.text(), "one ");
        buf.apply(&Edit::Yank);
        assert_eq!(buf.text(), "one two");
    }

    #[test]
    fn yank_inserts_at_the_cursor() {
        let mut buf = buffer("hello");
        buf.apply(&Edit::DeleteToStart);
        buf.apply(&Edit::Insert('x'));
        buf.apply(&Edit::Yank);
        assert_eq!(buf.text(), "xhello");
    }

    #[test]
    fn typing_inserts_at_the_cursor() {
        let mut buf = buffer("ac");
        buf.apply(&Edit::Move(Motion::Left));
        buf.apply(&Edit::Insert('b'));
        assert_eq!(buf.text(), "abc");
        assert_eq!(buf.cursor(), (0, 2));
    }

    #[test]
    fn a_newline_splits_the_line() {
        let mut buf = buffer("onetwo");
        buf.apply(&Edit::Move(Motion::Home));
        apply(
            &mut buf,
            &[
                Edit::Move(Motion::Right),
                Edit::Move(Motion::Right),
                Edit::Move(Motion::Right),
            ],
        );
        buf.apply(&Edit::Newline);
        assert_eq!(buf.text(), "one\ntwo");
        assert_eq!(buf.cursor(), (1, 0));
    }

    #[test]
    fn backspace_at_a_line_start_joins_the_lines() {
        let mut buf = buffer("one\ntwo");
        buf.apply(&Edit::Move(Motion::Home));
        buf.apply(&Edit::DeleteBackward);
        assert_eq!(buf.text(), "onetwo");
    }

    #[test]
    fn delete_forward_at_a_line_end_joins_the_lines() {
        let mut buf = buffer("one\ntwo");
        buf.apply(&Edit::Move(Motion::Up));
        buf.apply(&Edit::Move(Motion::End));
        buf.apply(&Edit::DeleteForward);
        assert_eq!(buf.text(), "onetwo");
    }

    #[test]
    fn multi_byte_characters_move_by_character() {
        let mut buf = buffer("héllo");
        buf.apply(&Edit::Move(Motion::Home));
        buf.apply(&Edit::Move(Motion::Right));
        buf.apply(&Edit::Insert('x'));
        assert_eq!(buf.text(), "hxéllo");
    }

    #[test]
    fn an_empty_buffer_reports_empty() {
        assert!(buffer("").is_empty());
        assert!(!buffer("x").is_empty());
    }
}
