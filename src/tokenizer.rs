use unicode_segmentation::UnicodeSegmentation;

/// Function used for tokenizing text data
pub fn tokenize(contents: &str) -> Vec<Vec<&str>> {

    // UnicodeSegmentation::unicode_sentences(contents) does not work 
    // because John F. Kennedy is split into two sentences

    let mut result = Vec::<Vec<&str>>::new();
    let mut sentence = Vec::<&str>::new();
    let mut sent_tokens = contents
        .unicode_words()
        .filter(|x| !x.is_empty()) // only tokens with non empty string len
        .collect::<Vec<&str>>();
    sentence.append(&mut sent_tokens);
    result.push(sentence);
    result
}
