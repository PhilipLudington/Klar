import * as vscode from "vscode";
import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | null = null;
let outputChannel: vscode.OutputChannel | null = null;

function resolveServerExecutable(): Executable {
    const config = vscode.workspace.getConfiguration("klar");
    const configuredPath = (config.get<string>("lsp.path") ?? "klar").trim();
    const extraArgs = config.get<string[]>("lsp.args") ?? [];

    return {
        command: configuredPath.length > 0 ? configuredPath : "klar",
        args: ["lsp", ...extraArgs],
        options: {},
    };
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    outputChannel = vscode.window.createOutputChannel("Klar LSP");
    context.subscriptions.push(outputChannel);

    const executable = resolveServerExecutable();
    const serverOptions: ServerOptions = executable;
    outputChannel.appendLine(`Starting Klar LSP: ${executable.command} ${executable.args.join(" ")}`);

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "klar" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.kl"),
        },
        outputChannel,
    };

    client = new LanguageClient(
        "klarLanguageServer",
        "Klar Language Server",
        serverOptions,
        clientOptions,
    );

    context.subscriptions.push(client.start());

    try {
        await client.onReady();
        outputChannel.appendLine("Klar LSP is ready.");
    } catch (err) {
        const message = `Failed to start Klar LSP: ${String(err)}`;
        outputChannel.appendLine(message);
        void vscode.window.showErrorMessage(message);
    }
}

export async function deactivate(): Promise<void> {
    if (client != null) {
        await client.stop();
        client = null;
    }
    if (outputChannel != null) {
        outputChannel.dispose();
        outputChannel = null;
    }
}
