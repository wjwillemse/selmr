#[test]
pub fn most_similar_test() {
    use selmr::selmr::SELMR;
    use selmr::text_structs::Text;
    use selmr::selmr::Params;
    let mut s = SELMR::new();
    let params = Params {
        min_phrase_len: 1,
        max_phrase_len: 3,
        min_left_context_len: 1,
        max_left_context_len: 3,
        min_right_context_len: 1,
        max_right_context_len: 3,
        min_phrase_keys: 1,
        min_context_keys: 1 ,
        language: "en".to_string(),
    };
    s.add("
        Here is Edward Bear, coming downstairs now, bump, bump, bump, on the back of his head, behind Christopher Robin.
        It is, as far as he knows, the only way of coming downstairs, but sometimes he feels that there really is another way,
        if only he could stop bumping for a moment and think of it. And then he feels that perhaps there isn't. Anyhow, here
        he is at the bottom, and ready to be introduced to you. Winnie-the-Pooh.",
        &params,
    );

    let actual = s.most_similar_phrase(Text::word("bump"), None, Some(15), Some(15), "count");

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
