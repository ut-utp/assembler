use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

// use dashmap::DashMap;
// use nrs_language_server::chumsky::{parse, type_inference, Func, ImCompleteSemanticToken, Spanned};
// use nrs_language_server::completion::completion;
// use nrs_language_server::jump_definition::{get_definition, get_definition_of_expr};
// use nrs_language_server::reference::get_reference;
// use nrs_language_server::semantic_token::{self, semantic_token_from_ast, LEGEND_TYPE};
use ropey::Rope;
// use serde::{Deserialize, Serialize};
// use serde_json::Value;
use tower_lsp::jsonrpc::{ErrorCode, Result};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, lsp_types, LspService, Server};
use lc3_assembler::{id, LeniencyLevel, parse_and_analyze};
use lc3_assembler::error::Error;
use lc3_assembler::parse::File;

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // TODO: Completion? (completion_provider)
                // TODO: Commands? (execute_command_provider)
                // TODO: Workspaces, linking (workspace)
                // TODO: Semantic highlighting (semantic_tokens_provider)
                //       ^ (could also do client-side syntax highlighting)
                // TODO: definition_provider, references_provider, rename_provider (at least for labels)
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(params.text_document.into())
            .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
            .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl From<lsp_types::TextDocumentItem> for TextDocumentItem {
    fn from(tdi: lsp_types::TextDocumentItem) -> Self {
        Self {
            uri: tdi.uri,
            text: tdi.text,
            version: tdi.version
        }
    }
}

fn diagnostics(error: &Error, rope: &Rope) -> Vec<Diagnostic> {
    match error {
        Error::Single(_, e) => vec![],
        Error::Spanned(span, e) => {
            let diagnostic = || -> Option<Diagnostic> {
                let start_position = offset_to_position(span.start(), &rope)?;
                let end_position = offset_to_position(span.end(), &rope)?;
                Some(Diagnostic::new_simple(
                    Range::new(start_position, end_position),
                    e.message(),
                ))
            }();
            diagnostic.into_iter().collect()
        }
        Error::Multiple(es) =>
            es.iter()
              .flat_map(|e| diagnostics(e, rope))
              .collect()
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = Rope::from_str(&params.text);
        let path = &params.uri.to_file_path().unwrap_or(PathBuf::from(""));
        let parse_result = parse_and_analyze(&id(&path), &params.text, LeniencyLevel::Lenient);

        let diagnostics =
            match parse_result {
                Ok(_) => vec![],
                Err(e) => diagnostics(&e, &rope),
            };

         self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) =
        LspService::build(|client|
            Backend {
                client,
            })
        .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}