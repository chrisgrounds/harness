//! The server uses unframed JSON objects and sends its history newest first.
use serde::Deserialize;
use std::io::{Read, Write};
use std::os::unix::net::UnixStream;

pub const DEFAULT_SOCKET: &str = "/tmp/harness.sock";
pub const MAX_REQUEST_BYTES: usize = 1024;

#[derive(Clone, Debug, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: Option<String>,
    #[serde(default, deserialize_with = "nullable_calls")]
    pub tool_calls: Vec<ToolCall>,
    pub tokens_per_second: Option<f64>,
}

fn nullable_calls<'de, D: serde::Deserializer<'de>>(d: D) -> Result<Vec<ToolCall>, D::Error> {
    Ok(Option::<Vec<ToolCall>>::deserialize(d)?.unwrap_or_default())
}

#[derive(Clone, Debug, Deserialize)]
pub struct ToolCall {
    pub function: Function,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Function {
    pub name: String,
    pub arguments: String,
}

#[derive(Debug, Deserialize)]
pub struct Reply {
    pub messages: Vec<Message>,
    pub model: Option<String>,
}

impl Reply {
    pub fn current_turn(mut self) -> Result<Self, String> {
        let end = self
            .messages
            .iter()
            .position(|m| m.role == "user")
            .unwrap_or(self.messages.len());
        self.messages.truncate(end);
        self.messages
            .retain(|m| m.role == "assistant" || m.role == "tool");
        if !self.messages.iter().any(|m| m.role == "assistant") {
            return Err("Server response contains no agent reply.".into());
        }
        self.messages.reverse();
        Ok(self)
    }
}

pub fn encode_request(input: &str) -> Vec<u8> {
    serde_json::to_vec(&serde_json::json!({"role": "user", "content": input}))
        .expect("serializing strings cannot fail")
}

pub fn validate_request(input: &str) -> Result<Vec<u8>, String> {
    if input.trim().is_empty() {
        return Err("Write a message first.".into());
    }
    let payload = encode_request(input);
    if payload.len() > MAX_REQUEST_BYTES {
        return Err(format!(
            "Message too long for the server ({} encoded bytes; maximum {MAX_REQUEST_BYTES}).",
            payload.len()
        ));
    }
    Ok(payload)
}

pub struct Session {
    stream: UnixStream,
    pending: Vec<u8>,
}

impl Session {
    pub fn new(stream: UnixStream) -> Self {
        Self {
            stream,
            pending: Vec::new(),
        }
    }

    pub fn send(&mut self, input: &str) -> Result<Reply, String> {
        let payload = validate_request(input)?;
        self.stream
            .write_all(&payload)
            .map_err(|e| format!("Could not send message: {e}"))?;
        self.receive()
    }

    fn receive(&mut self) -> Result<Reply, String> {
        loop {
            let mut parser =
                serde_json::Deserializer::from_slice(&self.pending).into_iter::<Reply>();
            match parser.next() {
                Some(Ok(reply)) => {
                    let consumed = parser.byte_offset();
                    self.pending.drain(..consumed);
                    return reply.current_turn();
                }
                Some(Err(e)) if !e.is_eof() => return Err(format!("Invalid server response: {e}")),
                _ => {}
            }
            let mut chunk = [0; 4096];
            let count = match self.stream.read(&mut chunk) {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                result => result.map_err(|e| format!("Could not read reply: {e}"))?,
            };
            if count == 0 {
                return Err("Server disconnected before a complete reply arrived.".into());
            }
            self.pending.extend_from_slice(&chunk[..count]);
        }
    }
}
