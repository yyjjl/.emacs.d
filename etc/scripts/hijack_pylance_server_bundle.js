const vm = require('node:vm');
const path = require('path');
const fs = require('fs');
const process = require('process');
const childProcess = require('child_process');
const assert = require('assert');

const scriptDir = resolveHome('~/.emacs.d/etc/scripts');
const outputDir = resolveHome('~/.emacs.d/.cache/lsp/pylance');

let espree = null;
try {
    espree = require('espree');
} catch (err) {
    process.chdir(scriptDir);
    console.log('>>>> Install espree ...');
    childProcess.execSync(`npm install espree`, { stdio: [0, 1, 2] });
    console.log('>>>> Install espree ...done');
    espree = require('espree');
}

function resolveHome(filepath) {
    if (filepath[0] === '~') {
        return path.join(process.env.HOME, filepath.slice(1));
    }
    return filepath;
}

function hijack(version) {
    function getCodeSegment({ start, end }) {
        return originalCode.slice(start, end);
    }

    function findAll(input, pattern) {
        const result = [];
        while (m = pattern.exec(input)) {
            result.push(m);
        }
        return result;
    }

    const workingDir = path.join(outputDir, version, 'extension/dist');
    const originalCode = fs.readFileSync(path.join(workingDir, 'server.bundle.js'), { encoding: 'utf-8' });
    const result = espree.parse(originalCode, { ecmaVersion: 'latest', range: true });

    assert(result.type == 'Program');
    assert(result.body.length == 3, 'Cannot parse code pattern');

    const functionDefs = result.body.filter(x => (x.type == 'FunctionDeclaration'));
    const mainStatement = result.body.filter(x => (x.type == 'ExpressionStatement'))[0];

    assert(functionDefs.length == 2, 'Cannot find two entry function defs');
    assert(mainStatement != undefined, 'Cannot find mainStatement');
    assert(mainStatement.expression.type == 'SequenceExpression', 'Cannot parse mainStatement (1)');
    assert(mainStatement.expression.expressions.length == 2, 'Cannot parse mainStatement (2)');

    const translateFnName = functionDefs[(functionDefs[0].params.length == 2) ? 0 : 1].id.name;

    const context = {};
    vm.createContext(context); // Contextify the object.
    vm.runInContext(getCodeSegment(functionDefs[0]), context);
    vm.runInContext(getCodeSegment(functionDefs[1]), context);
    vm.runInContext('(' + getCodeSegment(mainStatement.expression.expressions[0]) + ')', context);

    const tokenTranslator = context[translateFnName];

    let finalCode = null;

    const throwStatements = findAll(originalCode, /throw\s+new\s+Error(\([^;]+?\));/g);
    for (const match of throwStatements) {
        const text = espree.tokenize(match[1])
            .map(x => {
                if (x.type === 'String') {
                    return eval(x.value);
                }
                if (x.type === 'Numeric') {
                    try {
                        return tokenTranslator(x.value);
                    } catch {}
                }
                return '';
            })
            .join('');

        // console.log('>>>', text, '<<<');
        if (text.indexOf('licenseErrorText') != -1) {
            console.log(`Hijack!! replace "${match[0]}" => ";"`)
            finalCode = originalCode.slice(0, match.index) + ';' + originalCode.slice(match.index + match[0].length);
            break;
        }
    }

    return finalCode;
}


function main() {
    // const version = process.argv[1];
    const version = '2023.11.13';

    const finalCode = hijack(version);
    if (finalCode === null) {
        console.log('Failed to hijack server.bundle.js');
        return process.exit(1);
    }

    const outputFile = path.join(outputDir, version, 'extension/dist/server.bundle.hijack.js');
    const stream = fs.createWriteStream(outputFile);
    stream.once('open', (fd) => {
        console.log(`Write to file ${outputFile}`);
        stream.write(`
process.exit = (function(oldExit) {
    return function (_) {
        console.warn('Hijack (${version}) !!!');
        process.exit = oldExit; // restore
    }
}(process.exit));`);
        stream.write(finalCode);
        stream.end();
    });
}

main();
