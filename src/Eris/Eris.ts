import { CommandClient, ClientOptions, Message, PossiblyUncachedTextableChannel, TextableChannel, TextChannel, CommandGenerator, CommandOptions, AdvancedMessageContent } from 'eris';

interface DispatchableCommand {
    label: string,
    generator: CurriedGenerator,
    options: CommandOptions
}

type CurriedGenerator = (msg: Message<TextableChannel>) => (args: Array<String>) => () => void;

const uncurryGenerator = (generator: CurriedGenerator): CommandGenerator => {
    return (msg, args) => generator(msg)(args)();
};

export const _makeClient = (token: string) => (clientOptions: ClientOptions) => (prefix: string) => () => {
    return new CommandClient(token, clientOptions, {
        prefix: prefix
    });
};

export const _connectClient = (client: CommandClient) => () => {
    return client.connect().then(async () => {
        console.log(`Hachiya Shuto is now online.`);
    });
};

export const _onMessageCreate = (client: CommandClient) => (callback: (msg: Message<PossiblyUncachedTextableChannel>) => void) => () => {
    return client.on('messageCreate', msg => callback(msg))
};

export const _createTextMessage = (msg: Message<TextableChannel>) => (content: string) => () => {
    return msg.channel.createMessage({ content: content } as AdvancedMessageContent);
};

export const _editMessage = (msg: Message<TextChannel>) => (content: string) => () => {
    return msg.edit(content);
};

export const _registerCommands = (client: CommandClient) => (cmds: Array<DispatchableCommand>) => () => {
    cmds.forEach(cmd => client.registerCommand(cmd.label, uncurryGenerator(cmd.generator), cmd.options));
    return;
};