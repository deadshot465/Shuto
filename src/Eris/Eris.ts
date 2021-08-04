import { CommandClient, ClientOptions, Message, PossiblyUncachedTextableChannel, TextableChannel, TextChannel, CommandGenerator, CommandOptions, CommandClientOptions, EmbedOptions } from 'eris';

interface DispatchableCommand {
    label: string,
    generator: CurriedGenerator,
    options: CommandOptions
}

type CurriedGenerator = (msg: Message<TextableChannel>) => (args: Array<String>) => () => void;

const uncurryGenerator = (generator: CurriedGenerator): CommandGenerator => {
    return async (msg, args) => generator(msg)(args)();
};

export const _makeClient = (token: string) => (clientOptions: ClientOptions) => (commandClientOptions: CommandClientOptions) => () => {
    return new CommandClient(token, clientOptions, commandClientOptions);
};

export const _connectClient = (client: CommandClient) => () => {
    return client.connect().then(async () => {
        console.log(`Hachiya Shuto is now online.`);
    });
};

export const _onMessageCreate = (client: CommandClient) => (callback: (msg: Message<PossiblyUncachedTextableChannel>) => () => void) => () => {
    return client.on('messageCreate', msg => {
        callback(msg)()
    });
};

export const _onReady = (client: CommandClient) => (callback: () => void) => () => {
    return client.on('ready', () => callback());
};

export const _createTextMessage = (msg: Message<TextableChannel>) => (content: string) => () => {
    return msg.channel.createMessage(content).catch(reason => console.error(reason));
};

export const _createEmbed = (msg: Message<TextableChannel>) => (embed: EmbedOptions) => () => {
    return msg.channel.createMessage({ embed: embed }).catch(reason => console.error(reason));
};

export const _editMessage = (msg: Message<TextChannel>) => (content: string) => () => {
    return msg.edit(content);
};

export const _editStatus = (client: CommandClient) => (name: string) => () => {
    return client.editStatus('online', {
        name: name,
        type: 0
    });
};

export const _registerCommands = (client: CommandClient) => (cmds: Array<DispatchableCommand>) => () => {
    cmds.forEach(cmd => client.registerCommand(cmd.label, uncurryGenerator(cmd.generator), cmd.options));
};

export const _getTextChannel = (client: CommandClient) => (channelID: string) => () => {
    return client.getChannel(channelID);
}

export const _createChannelTextMessage = (channel: TextChannel) => (content: string) => () => {
    return channel.createMessage(content);
};

export const _createChannelEmbed = (channel: TextChannel) => (embed: EmbedOptions) => () => {
    return channel.createMessage({ embed: embed });
};