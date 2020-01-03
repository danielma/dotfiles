import { styled } from "uebersicht";

export const refreshFrequency = false; // 30000;

export const className = `
  left: 0px;
  right: 0px;
  bottom: 0px;
`;

const Bar = styled("div")`
  /* border-radius: 5px; */
  box-sizing: border-box;
  font-family: IBM Plex Mono;
  font-size: 14px;
  font-weight: normal;
  line-height: 1;
  display: flex;

  > * {
    padding: 10px;

    &:empty {
      padding: 0;
    }
  }

  /*
  > :first-child {
    border-top-left-radius: 5px;
    border-bottom-left-radius: 5px;
  }

  > :last-child {
    border-top-right-radius: 5px;
    border-bottom-right-radius: 5px;
  }
  */
`;

const Spacer = styled("div")`
  flex: 2;
`;

export function render() {
  return (
    <Bar id="bar" className="fg-base05 bg-base00-D0">
      <div className="inboxCount bg-base0C-FF fg-base02" />
      <div className="currentWindow" />
      <Spacer />
      <div className="musicBar bg-base0D-FF fg-base00" />
      <div className="time bg-base05-FF fg-base00" />
    </Bar>
  );
}
