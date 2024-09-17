// found on https://ascii.co.uk/art/trophy by a guy named jgs I guess
pub const ART: &str = r#"
   ___________
  '._==_==_=_.'
  .-\:      /-.
 | (|:.     |) |
  '-|:.     |-'
    \::.    /
     '::. .'
       ) (
     _.' '._
    `"""""""`
"#;

pub fn unlock_achievement(name: &str, message: &str) {
    println!("===[ Achievement unlocked: {} ]===", name);
    println!("{}", ART);
    println!(
        "{}
                                                                Congratulations!
================================================================================",
        message
    );
}

pub fn unlock_achievement_and_die(name: &str, message: &str) {
    unlock_achievement(name, message);
    panic!("===[ Achievement unlocked: {} ]===", name);
}
