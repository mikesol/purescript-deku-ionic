export const customComponentImpl =
    (componentName) => (localProps) => (connectedHook) => (disconnectedHook) => (run) => () => {
      console.log('doing');
    class CustomComponent extends HTMLElement {
      constructor() {
        super();
        console.log('fuck');
        this.ionic$locals = undefined;
      }
      disconnectedCallback() {
        disconnectedHook(this.ionic$locals)();
      }
      connectedCallback() {
        console.log('shit');
        this.ionic$locals = {};
        for (var i = 0; i < localProps.length; i++) {
          this.ionic$locals[localProps[i]] = this[localProps[i]];
        }
        run(this)(this.ionic$locals)();
        connectedHook(this.ionic$locals)();
      }
    }
    console.log('defining', componentName)
    window.customElements.define(componentName, CustomComponent);
  };
