#[test]
pub fn most_similar_test() {
    use selmr::selmr::SELMR;
    use selmr::text_structs::Text;
    use selmr::selmr::Params;
    let params = Params::test();
    let mut s = SELMR::new();
    s.add("
        Here is Edward Bear, coming downstairs now, bump, bump, bump, on the back of his head, behind Christopher Robin.
        It is, as far as he knows, the only way of coming downstairs, but sometimes he feels that there really is another way,
        if only he could stop bumping for a moment and think of it. And then he feels that perhaps there isn't. Anyhow, here
        he is at the bottom, and ready to be introduced to you. Winnie-the-Pooh.",
        Some(&params),
    );
    let actual = s.most_similar(Text::word("bump"), None, Some(15), Some(15), "count");
    match actual {
        Ok(actual) => {
            assert_eq!(
                actual,
                [(Text::word("bump"), 3.0), (Text::word("bump bump"), 2.0)]
            );
        }
        Err(_) => todo!(),
    }
}
