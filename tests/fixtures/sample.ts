export const GREETING = "hi";

export interface Point {
  x: number;
  y: number;
}

export class Widget {
  id: number;
  constructor(id: number) {
    this.id = id;
  }
  render(): string {
    return "w";
  }
}

export function greet(name: string): string {
  return `${GREETING} ${name}`;
}
