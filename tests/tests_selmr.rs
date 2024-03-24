#[test]
pub fn most_similar_test() {
    use selmr::selmr::SELMR;

    let mut s = SELMR::new(1, 3, 1, 3, 1, 1, "en");

    s.add("
        Here is Edward Bear, coming downstairs now, bump, bump, bump, on the back of his head, behind Christopher Robin.
        It is, as far as he knows, the only way of coming downstairs, but sometimes he feels that there really is another way,
        if only he could stop bumping for a moment and think of it. And then he feels that perhaps there isn't. Anyhow, here
        he is at the bottom, and ready to be introduced to you. Winnie-the-Pooh.
    ");

    let actual = s.most_similar("bump", None, 15, 15, 15);

    match actual {
        Ok(actual) => {
            assert_eq!(
                actual,
                [("bump".to_string(), 3), ("bump bump".to_string(), 2)]
            );
        }
        Err(_) => todo!(),
    }
}
