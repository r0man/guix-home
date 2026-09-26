/**
 * Lisp paren guard for pi.
 *
 * After every write/edit of a Lisp file, run lisp-paren-check on it.  If
 * the delimiters are unbalanced, append the checker's report (including
 * parenmedic's repair suggestion) to the tool result and mark it as an
 * error, so the model fixes the file before moving on.
 *
 * Installed by home-pi-service-type; @LISP_PAREN_CHECK@ is replaced by
 * the checker's store path at build time.
 */

import { execFile } from "node:child_process";
import * as os from "node:os";
import * as path from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const CHECKER = "@LISP_PAREN_CHECK@";

function resolvePath(raw: string, cwd: string): string {
	let p = raw.startsWith("@") ? raw.slice(1) : raw;
	if (p === "~" || p.startsWith("~/")) p = path.join(os.homedir(), p.slice(1));
	return path.resolve(cwd, p);
}

function check(file: string, signal?: AbortSignal): Promise<{ code: number; output: string }> {
	return new Promise((resolve) => {
		execFile(CHECKER, [file], { signal, maxBuffer: 1024 * 1024 }, (error, stdout, stderr) => {
			const code = error ? (typeof (error as any).code === "number" ? (error as any).code : 1) : 0;
			resolve({ code, output: `${stdout}${stderr}` });
		});
	});
}

export default function (pi: ExtensionAPI) {
	pi.on("tool_result", async (event, ctx) => {
		if (event.isError) return;
		if (event.toolName !== "write" && event.toolName !== "edit") return;
		const input = event.input as { path?: string; file_path?: string };
		const raw = input?.path ?? input?.file_path;
		if (typeof raw !== "string" || raw.length === 0) return;

		const { code, output } = await check(resolvePath(raw, ctx.cwd), ctx.signal);
		if (code !== 2) return;

		return {
			content: [
				...event.content,
				{
					type: "text",
					text: `\n${output}\nThe file was saved, but its delimiters are unbalanced. Fix them now before doing anything else.`,
				},
			],
			isError: true,
		};
	});
}
