use rs_gui_client::protocol::{Session, encode_request, validate_request};
use serde_json::{Value, json};
use std::{
    io::{Read, Write},
    os::unix::net::{UnixListener, UnixStream},
    thread,
    time::Duration,
};

fn read_request(stream: &mut UnixStream) -> Value {
    let mut bytes = Vec::new();
    loop {
        let mut chunk = [0; 1024];
        let n = stream.read(&mut chunk).unwrap();
        assert_ne!(n, 0, "client closed before sending a request");
        bytes.extend_from_slice(&chunk[..n]);
        if let Ok(value) = serde_json::from_slice(&bytes) {
            return value;
        }
    }
}

fn connected_pair() -> (UnixStream, UnixStream) {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("harness.sock");
    let listener = UnixListener::bind(&path).unwrap();
    let client = UnixStream::connect(path).unwrap();
    let (server, _) = listener.accept().unwrap();
    for stream in [&client, &server] {
        stream
            .set_read_timeout(Some(Duration::from_secs(5)))
            .unwrap();
        stream
            .set_write_timeout(Some(Duration::from_secs(5)))
            .unwrap();
    }
    (client, server)
}

#[test]
fn fragmented_unicode_tool_cycles_and_history() {
    let (client, mut server) = connected_pair();
    let large_output = "café λ\n  indented\ttext ".repeat(1000);
    let output = large_output.clone();
    let worker = thread::spawn(move || {
        let mut history = Vec::new();
        for (prompt, responses) in [
            (
                "read files",
                vec![
                    json!({"role":"assistant","tool_calls":[{"function":{"name":"ReadFile","arguments":"{\"path\":\"a.hs\"}"}}]}),
                    json!({"role":"tool","content":output}),
                    json!({"role":"assistant","content":"Reading another file.","tokens_per_second":150.25}),
                    json!({"role":"assistant","tool_calls":[{"function":{"name":"ReadFile","arguments":"{\"path\":\"b.hs\"}"}}]}),
                    json!({"role":"tool","content":"second file"}),
                    json!({"role":"assistant","content":"Both files read."}),
                ],
            ),
            (
                "thanks",
                vec![
                    json!({"role":"assistant","content":"You're welcome.","tool_calls":null,"tokens_per_second":null}),
                ],
            ),
        ] {
            let incoming = read_request(&mut server);
            assert_eq!(incoming, json!({"role":"user","content":prompt}));
            history.push(incoming);
            history.extend(responses);
            let payload = serde_json::to_vec(
                &json!({"messages":history.iter().rev().collect::<Vec<_>>(),"model":"mercury-3"}),
            )
            .unwrap();
            for chunk in payload.chunks(1) {
                server.write_all(chunk).unwrap();
            }
        }
    });
    let mut session = Session::new(client);
    let reply = session.send("read files").unwrap();
    assert_eq!(reply.model.as_deref(), Some("mercury-3"));
    assert_eq!(reply.messages.len(), 6);
    assert_eq!(reply.messages[0].tool_calls[0].function.name, "ReadFile");
    assert_eq!(
        reply.messages[1].content.as_deref(),
        Some(large_output.as_str())
    );
    assert_eq!(reply.messages[2].tokens_per_second, Some(150.25));
    assert_eq!(
        reply.messages[5].content.as_deref(),
        Some("Both files read.")
    );
    let next = session.send("thanks").unwrap();
    assert_eq!(next.messages.len(), 1);
    assert_eq!(next.messages[0].content.as_deref(), Some("You're welcome."));
    assert!(next.messages[0].tokens_per_second.is_none());
    worker.join().unwrap();
}

#[test]
fn coalesced_replies_preserve_unconsumed_bytes() {
    let (client, mut server) = connected_pair();
    let worker = thread::spawn(move || {
        read_request(&mut server);
        server.write_all(br#" {"messages":[{"role":"assistant","content":"first"}]} {"messages":[{"role":"assistant","content":"second"}]} "#).unwrap();
        read_request(&mut server);
    });
    let mut session = Session::new(client);
    assert_eq!(
        session.send("one").unwrap().messages[0].content.as_deref(),
        Some("first")
    );
    assert_eq!(
        session.send("two").unwrap().messages[0].content.as_deref(),
        Some("second")
    );
    worker.join().unwrap();
}

#[test]
fn invalid_empty_and_truncated_replies_fail_cleanly() {
    for (payload, expected) in [
        (b"not json".as_slice(), "Invalid server response"),
        (br#"{"messages":[]}"#.as_slice(), "no agent reply"),
        (
            br#"{"messages":[{"role":"tool","content":"orphan"}]}"#.as_slice(),
            "no agent reply",
        ),
        (br#"{"messages":["#.as_slice(), "disconnected"),
        (b"".as_slice(), "disconnected"),
    ] {
        let (client, mut server) = connected_pair();
        let worker = thread::spawn(move || {
            read_request(&mut server);
            server.write_all(payload).unwrap();
        });
        assert!(
            Session::new(client)
                .send("hello")
                .unwrap_err()
                .contains(expected)
        );
        worker.join().unwrap();
    }
}

#[test]
fn request_limit_counts_encoded_utf8_and_escaping() {
    let overhead = encode_request("").len();
    assert_eq!(
        validate_request(&"a".repeat(1024 - overhead))
            .unwrap()
            .len(),
        1024
    );
    assert!(validate_request(&"a".repeat(1025 - overhead)).is_err());
    assert!(validate_request(&"λ".repeat(512)).is_err());
    assert!(validate_request(&"\"".repeat(512)).is_err());
    assert!(validate_request(" \n\t ").is_err());
}

#[test]
fn rejected_input_does_not_write_to_socket() {
    let (client, mut server) = connected_pair();
    let worker = thread::spawn(move || {
        assert_eq!(
            read_request(&mut server),
            json!({"role":"user","content":"valid"})
        );
        server
            .write_all(br#"{"messages":[{"role":"assistant","content":"ok"}]}"#)
            .unwrap();
    });
    let mut session = Session::new(client);
    assert!(session.send(&"x".repeat(1024)).is_err());
    assert!(session.send(" ").is_err());
    assert!(session.send("valid").is_ok());
    worker.join().unwrap();
}
